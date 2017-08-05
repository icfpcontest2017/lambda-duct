(*** lambda duct ***)

(* Offline mode simulation for a single punter *)

(* each string is prefixed by its size: <size>:<string> *)
module BoundedIO = struct
  exception MessageTooBig of int
  exception BadCharacter of char
  exception SigPipe

  open Lwt
  let send oc s =
    (* HACK: two flushed writes are apparently sufficient to guarantee
       that an EPIPE exception is raised if the output channel has
       been closed:

         https://github.com/mirage/ocaml-cohttp/issues/57
    *)
    Lwt_log.debug_f "BIO send: %s." s >>= fun () ->
    Lwt_io.write oc (string_of_int @@ String.length s) >>= fun () ->
    Lwt_io.write_char oc ':' >>= fun () ->
    Lwt_io.flush oc >>= fun () ->
    Lwt_io.write oc s >>= fun () ->
    Lwt_io.flush oc
  let recv ic =
    let max_size = 9 in
    let size_buf = Buffer.create max_size in
    let rec read_size count =
      (* don't allow more than 9 digits *)
      if count > max_size then
        fail (MessageTooBig max_size)
      else
        Lwt_io.read_char ic >>= function
        | c when '0' <= c && c <= '9' ->
           Buffer.add_char size_buf c;
           read_size (count+1)
        | ':' ->
           return (int_of_string @@ Buffer.contents size_buf)
        | c ->
           (*prerr_endline @@ "Bad character: " ^ String.make 1 c;*)
           fail (BadCharacter c) in
    Lwt_log.debug "BIO: receiving." >>= fun () ->
    read_size 0 >>= fun size ->
    let data_buf = Bytes.create size in
    Lwt_io.read_into_exactly ic data_buf 0 size >>= fun () ->
    let msg = Bytes.to_string data_buf in
    Lwt_log.debug_f "BIO recv: %s." msg
    >>= fun () ->
    return msg
end

(* File abstraction *)
module File = struct
  type t = string

  let sep = '/'

  let absolute_path filename =
    let open Filename in
    let filename =
      if is_relative filename
      then concat (Sys.getcwd ()) filename
      else filename
    in
  (* Simplify . and .. components *)
    let rec simplify filename =
      let base = basename filename in
      let dir = dirname filename in
      if dir = filename then dir
      else if base = current_dir_name then simplify dir
      else if base = parent_dir_name then dirname (simplify dir)
      else concat (simplify dir) base
    in
    simplify filename

  let of_string : string -> t
    = absolute_path

  (* let rec intersperse : 'a -> 'a list -> 'a list *)
  (*   = fun y -> *)
  (*     function *)
  (*     | [] -> [] *)
  (*     | [x] -> [x] *)
  (*     | x :: xs -> x :: y :: (intersperse y xs);; *)

  (* let to_string : t -> string *)
  (*   = fun f -> *)
  (*     let sep = String.make 1 sep in *)
  (*     List.fold_right *)
  (*       (fun p acc -> p ^ acc) *)
  (*       (intersperse sep f) "" *)

  let to_string x = x

  let exists : t -> bool
    = fun f ->
      Sys.file_exists (to_string f)

  let basename = Filename.basename
  let dirname  = Filename.dirname
  (* let basename : t -> string *)
  (*   = fun f -> *)
  (*     match List.rev f with *)
  (*     | [] -> "" *)
  (*     | x :: _ -> x *)

  (* let dirname : t -> string *)
  (*   = fun f -> *)
  (*     match List.rev f with *)
  (*     | [] *)
  (*     | [_] -> "." *)
  (*     | _ :: ps -> to_string (List.rev ps) *)
end

(* Remote host resolver *)
module Resolve = struct
  exception Unresolved of string
  type resolved_host = {
    inet_address: Unix.inet_addr;
    address_type: Unix.socket_domain;
  }

  let string_of_resolved_host rh = Unix.string_of_inet_addr rh.inet_address

  let by_name : string -> resolved_host
    = fun hostname ->
      try
        let open Unix in
        let host = gethostbyname hostname in
        if Array.length host.h_addr_list > 0
        then { inet_address = host.h_addr_list.(0);
               address_type = host.h_addrtype; }
        else raise (Unresolved hostname)
      with Not_found -> raise (Unresolved hostname)
end

(* Json *)
module Json = struct
  exception MalformedJson of string
  exception AbsentField of string

  type t = Yojson.Basic.json

  let to_string : t -> string
    = fun json -> Yojson.Basic.to_string json
  let from_string : string -> t
    = fun s ->
      try Yojson.Basic.from_string s with
      | Yojson.Json_error s -> raise (MalformedJson s)

  let from_key_value_pair : (string * t) -> t
    = fun p -> `Assoc [p]

  let union : t -> t -> t
    = fun x y ->
    try Yojson.Basic.Util.combine x y
    with Yojson.Basic.Util.Type_error (s,json) -> raise (MalformedJson (Printf.sprintf "%s, %s" s (to_string json)))

  let pop : string -> t -> (t * t)
    = fun field ->
      function
      | `Assoc xs ->
         begin
           try
             List.assoc field xs, `Assoc (List.remove_assoc field xs)
           with Not_found -> raise (AbsentField (field))
         end
      | _ -> raise (AbsentField (field))

  let present : string -> t -> bool
    = fun field ->
      function
      | `Assoc xs -> List.mem_assoc field xs
      | _ -> false
end

(* Lambda Punter Message *)
module LPMessage = struct
  type t = Json.t

  type kind =
      Me | You | Move | Ready | Punter | Stop | Timeout | Unknown

  let string_of_kind = function
    | Me -> "me" | You -> "You" | Move -> "move" | Ready -> "ready"
    | Punter -> "punter" | Stop -> "stop" | Timeout -> "timeout"
    | Unknown -> "<unknown message kind>"

  exception AmbigiousMessage of kind list
  exception BadMessage of kind * kind

  let augment : (string * t) -> t -> t
    = fun p msg ->
      Json.union msg (Json.from_key_value_pair p)

  let pop : key:string -> t -> (t * t)
    = fun ~key msg -> Json.pop key msg

  let kind : t -> kind
    = fun msg ->
      let xs =
        List.map
          (fun (k,f) -> (k, Json.present f msg))
          [(Me, "me"); (You, "you"); (Move, "move"); (Ready, "ready"); (Punter, "punter"); (Stop, "stop"); (Timeout, "timeout")]
      in
      match List.filter snd xs with
      | []         -> Unknown
      | [(kind,_)] -> kind
      | kinds -> raise (AmbigiousMessage (List.map fst kinds))

  let from_string = Json.from_string
  let to_string   = Json.to_string

  let me : string -> t
    = fun name ->  `Assoc [("me", `String name)]

  let empty : t = Json.from_string "{}"

  let you_of_me : t -> t
    = fun msg ->
      match kind msg with
      | Me ->
         let (me,_) = pop ~key:"me" msg in
         augment ("you", me) empty
      | k -> raise (BadMessage (Me, k))
end

(* Bounded Bidirectional Channel *)
module Chan: sig
  type t
  type msg = LPMessage.t

  val make : Lwt_io.input_channel -> Lwt_io.output_channel -> t
  val recv : t -> msg Lwt.t
  val send : t -> msg -> unit Lwt.t
  val close : t -> unit Lwt.t

  val default : t

end = struct
  type t = { ic: Lwt_io.input_channel;
             oc: Lwt_io.output_channel;
             close: (unit -> unit Lwt.t);
           }

  type msg = LPMessage.t

  let make ic oc =
    { ic; oc;
      close = (fun () -> Lwt.(Lwt_io.close ic >>= fun () ->
                              Lwt_io.close oc)); }
  let recv ch =
    let open Lwt in
    BoundedIO.recv ch.ic
    >>= fun msg ->
    return @@ LPMessage.from_string msg

  let send ch msg =
    BoundedIO.send ch.oc (LPMessage.to_string msg)

  let close ch =
    ch.close ()

  let default = make Lwt_io.zero Lwt_io.null
end

module Proc: sig
  type t
  type status =
      Running | Exited of Unix.process_status

  (* Copy semantics for stdin, -out, and -err *)
  val spawn : ?timeout:float ->
              ?stdin:Lwt_process.redirection ->
              ?stdout:Lwt_process.redirection ->
              ?stderr:Lwt_process.redirection ->
              ?args:string list -> string -> t

  val terminate : t -> unit
  val empty : t
  val status : t -> status
  val string_of_status : status -> string
  val wait : t -> status Lwt.t
end = struct
  type t = Empty
         | Proc of Lwt_process.process_none

  type status =
      Running | Exited of Unix.process_status

  let spawn ?(timeout=0.0) ?(stdin=`Dev_null) ?(stdout=`Dev_null) ?(stderr=`Dev_null) ?(args=[]) prog =
    Proc
      (Lwt_process.open_process_none
         ~timeout ~stdin ~stdout ~stderr ("", Array.of_list (prog :: args)))

  let terminate = function
    | Empty -> ()
    | Proc proc -> proc#terminate

  let empty = Empty

  module Signals = struct
    open Sys
    let string_of_signal = function
      | n when n = sigabrt   -> "Abnormal termination"
      | n when n = sigalrm   -> "Timeout"
      | n when n = sigfpe    -> "Arithmetic exception"
      | n when n = sighup    -> "Hangup on controlling terminal"
      | n when n = sigill    -> "Invalid hardware instruction"
      | n when n = sigint    -> "Interactive interrupt (ctrl-C)"
      | n when n = sigkill   -> "Termination (cannot be ignored)"
      | n when n = sigpipe   -> "Broken pipe"
      | n when n = sigquit   -> "Interactive termination"
      | n when n = sigsegv   -> "Invalid memory reference"
      | n when n = sigterm   -> "Termination"
      | n when n = sigusr1   -> "Application-defined signal 1"
      | n when n = sigusr2   -> "Application-defined signal 2"
      | n when n = sigchld   -> "Child process terminated"
      | n when n = sigcont   -> "Continue"
      | n when n = sigstop   -> "Stop"
      | n when n = sigtstp   -> "Interactive stop"
      | n when n = sigttin   -> "Terminal read from background process"
      | n when n = sigttou   -> "Terminal write from background process"
      | n when n = sigvtalrm -> "Timeout in virtual time"
      | n when n = sigprof   -> "Profiling interrupt"
      | n when n = sigbus    -> "Bus error"
      | n when n = sigpoll   -> "Pollable event"
      | n when n = sigsys    -> "Bad argument to routine"
      | n when n = sigtrap   -> "Trace/breakpoint trap"
      | n when n = sigurg    -> "Urgent condition on socket"
      | n when n = sigxcpu   -> "Timeout in cpu time"
      | n when n = sigxfsz   -> "File size limit exceeded"
      | _                    -> "Unknown reason"
  end

  let string_of_status = function
    | Running -> "Running"
    | Exited st ->
       match st with
       | Unix.WEXITED code -> Printf.sprintf "Exited with code %d" code
       | Unix.WSIGNALED signal | Unix.WSTOPPED signal -> Signals.string_of_signal signal

  let status = function
    | Empty -> Exited (Unix.WEXITED 0)
    | Proc p ->
       match p#state with
       | Lwt_process.Running -> Running
       | Lwt_process.Exited st -> Exited st

  let wait = function
    | Empty  -> Lwt.return (Exited (Unix.WEXITED 0))
    | Proc p -> Lwt.(p#status >>= fun st -> return (Exited st))
end

(* Mediates communication between offline mode punter and online mode
   lambda punter server. *)
module Mediator: sig
  exception ConnectionFailure
  exception ClientProgramNotFound of string
  exception ClientInteractionError of exn * Proc.status
  exception ServerInteractionError of exn

  type program = File.t
  val simulate : hostname:string -> port:int -> timeout:float -> response_timeout:float -> client_log:Unix.file_descr -> program -> unit Lwt.t
end = struct
  exception ConnectionFailure

  type chan = Chan.t

  type client = {
    cl_name: string;
    cl_prog: File.t;
    cl_state: LPMessage.t;
    cl_proc: Proc.t;
    cl_ch: chan;
  }

  type server = {
    srv_fd: Lwt_unix.file_descr;
    srv_ch: chan;
  }

  type state = {
    client: client;
    server: server;
  }

  let recv : chan -> LPMessage.t Lwt.t
    = fun ch -> Chan.recv ch

  let send : chan -> LPMessage.t -> unit Lwt.t
    = fun ch msg -> Chan.send ch msg

  type program = File.t

  let make_client : ?proc:Proc.t ->
                    ?state:LPMessage.t ->
                    ?chan:Chan.t -> string -> program -> client
    = fun ?(proc=Proc.empty) ?(state=LPMessage.empty) ?(chan=Chan.default) name prog ->
      { cl_prog  = prog;
        cl_state = state;
        cl_ch    = chan;
        cl_proc  = proc;
        cl_name  = name }

  let update_client : ?name:string ->
                      ?state:LPMessage.t ->
                      ?proc:Proc.t  ->
                      ?chan:chan    -> client -> client
    = fun ?name ?state ?proc ?chan cl ->
      let from_some default = function
        | Some x -> x
        | None   -> default
      in
      let proc = from_some cl.cl_proc proc in
      let state = from_some cl.cl_state state in
      let chan  = from_some cl.cl_ch chan in
      let name  = from_some cl.cl_name name in
      make_client ~proc ~state ~chan name cl.cl_prog


  let timed : float -> 'a Lwt.t -> 'a Lwt.t
    = fun d f -> Lwt.pick [Lwt_unix.timeout d; f]

  exception ClientProgramNotFound of string
  exception ClientInteractionError of exn * Proc.status
  let cl_interact : float -> Proc.t -> (unit -> 'a Lwt.t) -> 'a Lwt.t
    = fun d proc f ->
      match Proc.status proc with
      | Proc.Running ->
         Lwt.catch (fun () ->
           try
             timed d (f ())
           with Lwt_unix.Timeout -> raise Not_found
         )
           (fun e -> Lwt.fail (ClientInteractionError (e, Proc.status proc)))
      | pstatus -> Lwt.fail (ClientInteractionError (Not_found, pstatus))

  exception ServerInteractionError of exn
  let srv_interact : (unit -> 'a Lwt.t) -> 'a Lwt.t
    = fun f ->
      Lwt.catch f
        (fun e -> Lwt.fail (ServerInteractionError e))

  let simulate ~hostname ~port ~timeout ~response_timeout ~client_log program =
    let open Lwt in
    let initial_state srv_fd srv_ch =
      { client = make_client "No name" program;
        server = { srv_fd; srv_ch; }; }
    in
    let stderr = `FD_copy client_log in
    (* connect to game server *)
    let connect hostname port =
      Lwt_log.debug (Printf.sprintf "Resolving %s..." hostname)
      >>= fun () ->
      let host = Resolve.by_name hostname in
      let open Resolve in
      Lwt_log.debug (Printf.sprintf "... resolved to %s." (string_of_resolved_host host))
      >>= fun () ->
      Lwt_log.debug "Creating TCP connection..."
      >>= fun () ->
      let fd =
        Lwt_unix.socket host.address_type Unix.SOCK_STREAM 0  (* 0 selects the default protocol *)
      in
      let () = (* configure socket *)
        Lwt_unix.setsockopt fd Unix.SO_REUSEADDR true;
        Lwt_unix.set_blocking fd false (* underlying socket setting *)
      in
      let ic = Lwt_io.of_fd ~mode:Lwt_io.Input fd in
      let oc = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
      Lwt_log.info (Printf.sprintf "Connecting to %s:%d..." hostname port)
      >>= fun () ->
        Lwt.catch
          (fun () -> Lwt_unix.connect fd (Unix.ADDR_INET (host.inet_address, port)))
          (fun _ -> Lwt.fail ConnectionFailure)
      >>= fun () ->
      Lwt_log.debug "Connection established."
      >>= fun () ->
      return (initial_state fd (Chan.make ic oc))
    in
    (* await turn *)
    let await_turn st =
      Lwt_log.info "Awaiting turn..."
      >>= fun () ->
      srv_interact (fun () -> recv st.server.srv_ch)
      >>= fun msg ->
      Lwt_log.debug_f "Game server: %s" (LPMessage.to_string msg)
      >>= fun () ->
      return (msg, st)
    in
    (* invoke client program *)
    let invoke_client (msg, st) =
      Lwt_log.info (Printf.sprintf "Invoking program %s..." (File.to_string st.client.cl_prog))
      >>= fun () ->
      if File.exists st.client.cl_prog then
        begin
          let cwd = Unix.getcwd () in
          let pdir = File.dirname st.client.cl_prog in
          Lwt_log.debug_f "Changing working directory to: %s..." pdir
          >>= fun () ->
          Unix.chdir pdir;
          let stdin_pipe = Lwt_unix.pipe_out () in
          let stdout_pipe = Lwt_unix.pipe_in () in
          let icr = fst stdin_pipe in
          let ocr = snd stdout_pipe in
          let proc =
            Proc.spawn
              ~timeout
              ~stdin:(`FD_move icr)
              ~stdout:(`FD_move ocr)
              ~stderr
              (Printf.sprintf "./%s" (File.basename st.client.cl_prog))
          in
          let chan =
            Chan.make
              (Lwt_io.of_fd ~mode:Lwt_io.Input (fst stdout_pipe))
              (Lwt_io.of_fd ~mode:Lwt_io.Output (snd stdin_pipe))
          in
          Lwt_log.debug_f "Changing working directory to: %s..." cwd
          >>= fun () ->
          Unix.chdir cwd;
          return (msg, { st with client = (update_client ~proc ~chan st.client) })
        end
      else Lwt.fail (ClientProgramNotFound (File.to_string st.client.cl_prog))
    in
    (* sends a message to the client *)
    let send_message_to_client (msg, st) =
      Lwt_log.debug "Forwarding message to client..."
      >>= fun () ->
      Lwt_log.debug_f "... message: %s." (LPMessage.to_string msg)
      >>= fun () ->
      cl_interact response_timeout st.client.cl_proc (fun () -> send st.client.cl_ch msg)
      >>= fun () ->
      Lwt_log.debug "Message has been forwarded."
      >>= fun () ->
      return st
    in
    (* sends combined moves & client state *)
    let send_message_with_state_to_client (msg, st) =
      let msg = LPMessage.augment ("state", st.client.cl_state) msg in
      send_message_to_client (msg, st)
    in
    (* receive move and state from client *)
    let receive_state_from_client st =
      Lwt_log.info "Awaiting move from client program..."
      >>= fun () ->
      cl_interact response_timeout st.client.cl_proc (fun () -> recv st.client.cl_ch)
      (* recv st.client.cl_ch *)
      >>= fun msg ->
      Lwt_log.debug_f "Client: %s" (LPMessage.to_string msg)
      >>= fun () ->
      try
        let (state, move) = LPMessage.pop ~key:"state" msg in
        return (msg, { st with client = update_client ~state st.client; })
      with
      | e -> Lwt.fail e
    in
    (* kill client *)
    let kill_client (msg, st) =
      Lwt_log.info "Terminating client program..."
      >>= fun () ->
      Proc.terminate st.client.cl_proc;
      Lwt_log.debug "Client program has been terminated."
      >>= fun () ->
      Chan.close st.client.cl_ch
      >>= fun () ->
      return (msg, { st with client = update_client ~proc:Proc.empty st.client ~chan:Chan.default })
    in
    (* forward client move *)
    let forward_client_move_to_server (msg, st) =
      Lwt_log.info "Forwarding client move..."
      >>= fun () ->
      srv_interact (fun () -> send st.server.srv_ch msg)
      >>= fun () ->
      Lwt_log.debug "Move has been forwarded."
      >>= fun () ->
      return st
    in
    (* Initial handshake between offline client and online server *)
    let handshake st =
      (* Handshake with online game server *)
      let server_handshake (me, st) =
        Lwt_log.info "Handshaking with server..."
        >>= fun () ->
        Lwt_log.debug_f "Sending %s to server" (LPMessage.to_string me)
        >>= fun () ->
        srv_interact (fun () -> send st.server.srv_ch me)
        >>= fun () ->
        srv_interact (fun () -> recv st.server.srv_ch)
        >>= fun you ->
        Lwt_log.debug_f "Game server: %s" (LPMessage.to_string you)
        >>= fun () ->
        return (you, st)
      in
      Lwt_log.info "Handshaking with client..."
      >>= fun () ->
      invoke_client (LPMessage.empty, st)
      >>= fun (_,st) ->
      Lwt_log.debug "Interacting with client..."
      >>= fun () ->
      cl_interact response_timeout st.client.cl_proc (fun () -> recv st.client.cl_ch)
      >>= fun me ->
      Lwt_log.debug_f "Me: %s." (LPMessage.to_string me)
      >>= fun () ->
      server_handshake (me, st)
      >>= fun (you, st) ->
      Lwt_log.debug_f "You: %s." (LPMessage.to_string you)
      >>= fun () ->
      cl_interact response_timeout st.client.cl_proc (fun () -> send st.client.cl_ch you)
      >>= fun () ->
      kill_client (LPMessage.empty, st)
      >>= fun (_,st) ->
      return st
    in
    (* Handshake with offline client *)
    let simulate_offline_handshake (msg, st) =
      Lwt_log.info "Handshaking with client..."
      >>= fun () ->
      cl_interact response_timeout st.client.cl_proc (fun () -> recv st.client.cl_ch)
      >>= fun me ->
      Lwt_log.debug "Simulating handshake with game server..."
      >>= fun () ->
      let you = LPMessage.you_of_me me in
      Lwt_log.debug_f "... sending: %s." (LPMessage.to_string you)
      >>= fun () ->
      cl_interact response_timeout st.client.cl_proc (fun () -> send st.client.cl_ch you)
      >>= fun () ->
      return (msg, st)
    in
    (* Construct loop *)
    let rec loop st =
      let loop' st =
        invoke_client st
        >>= simulate_offline_handshake
        >>= send_message_with_state_to_client
        >>= receive_state_from_client
        >>= kill_client
        >>= forward_client_move_to_server
        >>= loop (* recurse *)
      in
      await_turn st
      >>= (fun (msg, st) ->
        let open LPMessage in
        match LPMessage.kind msg with
        | Stop ->
           Lwt_log.info "Game ended. Notifying client program..."
           >>= fun () ->
           (invoke_client (msg, st))
           >>= simulate_offline_handshake
           >>= send_message_with_state_to_client
           >>= fun st ->
           catch
             (fun () ->
               (* Lwt_unix.sleep 0.25 *)
               (* >>= fun () -> *)
             (* kill_client (msg, st) *)
               Lwt_log.info "Waiting on client program to exit..." >>= fun () ->
               Proc.wait st.client.cl_proc >>= fun pstatus ->
               Lwt_log.info_f "Client %s." (Proc.string_of_status pstatus) >>= fun () ->
               return (msg, st)
             )
             (fun _ -> return (msg, st))
           >>= (fun (_,st) -> return st)
        | Unknown ->
           Lwt_log.warning "unknown message kind; continuing..."
           >>=  fun () -> loop' (msg, st)
        | _ -> loop' (msg, st))
    in
    (connect hostname port)
    >>= handshake
    >>= loop
    >>= fun _ -> return_unit
end

module Settings = struct
  let game_hostname = ref "punter.inf.ed.ac.uk"
  let game_port     = ref 9999

  let client_program : string option ref = ref None
  let client_instance_timeout : float ref = ref 10.0
  let client_response_timeout : float ref = ref 10.0
  let client_instance_log : string ref = ref "/dev/null"
end

let string_of_list : ('a -> string) -> 'a list -> string
  = fun string_of_x xs ->
    let rec string_of_list = function
      | []  -> ""
      | [x] -> string_of_x x
      | x :: xs -> Printf.sprintf "%s, %s" (string_of_x x) (string_of_list xs)
    in
    Printf.sprintf "[%s]" (string_of_list xs)

let _ =
  let print_version () =
    Printf.fprintf stdout "Lambda Duct version %s+%s (%s)\n%!" Version.number Version.git Version.name;
    exit 0
  in
  let open Arg in
  let set_client client =
    match !Settings.client_program with
    | None      -> Settings.client_program := Some client
    | Some prog -> raise (Bad (Printf.sprintf "error: client program '%s' supplied, but another client program '%s' is already registered. Only a single client program is supported" client prog))
  in
  let set_port = function
    | n when n > 0 -> Settings.game_port := n
    | _            -> raise (Bad "error: port must be a positive integer")
  in
  let set_logging level =
    ignore(if level > 0 then Lwt_log.add_rule "*" Lwt_log.Warning);
    ignore(if level > 1 then Lwt_log.add_rule "*" Lwt_log.Info);
    ignore(if level > 2 then Lwt_log.add_rule "*" Lwt_log.Debug)
  in
  let set_timeout setting f =
    if f > 0.0 then setting := f
    else raise (Bad "error: client instance timeout must be positive")
  in
  let arg_specs = align [
    ("--client-instance-logfile", Set_string Settings.client_instance_log,    " Logging client instance stderr               (default: /dev/null)");
    ("--client-instance-timeout", Float (fun f -> set_timeout Settings.client_instance_timeout f),             " Maximum lifetime per client program instance (default: " ^ (Printf.sprintf "%.0f seconds)" !Settings.client_instance_timeout));
    ("--client-response-timeout", Float (fun f -> set_timeout Settings.client_response_timeout f),             " Maximum time for client program to respond   (default: " ^ (Printf.sprintf "%.0f seconds)" !Settings.client_response_timeout));
    ("--game-hostname",           Set_string Settings.game_hostname,          " Hostname of the game server                  (default: " ^ !Settings.game_hostname ^ ")" );
    ("--game-port",               Int (fun n -> set_port n),                  " Port to connect to on the game server        (default: " ^ (string_of_int !Settings.game_port) ^ ")");
    ("--log-level",               Int (fun n -> set_logging n),               " Logging level for lamduct (values: 0 to 3)   (default: 0)");
    ("--version",                 Unit print_version,                         " Print version and exit");
  ]
  in
  let usage = "usage: lamduct [options] <client program>\n" in
  let _ = Arg.parse arg_specs (fun client -> set_client client) (Printf.sprintf "%sOptions are:" usage) in
  match !Settings.client_program with
  | None -> Printf.fprintf stderr "%s" usage; exit 2
  | Some prog ->
     let log_fd =
       try
         Unix.openfile !Settings.client_instance_log [Unix.O_CREAT; Unix.O_WRONLY; Unix.O_APPEND] 0o644
       with
       | Unix.Unix_error (_,_,_) ->
         Printf.fprintf stderr "error: could open file %s.\n%!" !Settings.client_instance_log;
         exit 1
     in
     try
       let open Lwt in
       Lwt_main.run @@
         Lwt.catch
         (fun () ->
           Mediator.simulate
             ~hostname:!Settings.game_hostname
             ~port:!Settings.game_port
             ~timeout:!Settings.client_instance_timeout
             ~response_timeout:!Settings.client_response_timeout
             ~client_log:log_fd
             (File.of_string prog))
         (fun exn ->
           (match exn with
           | BoundedIO.MessageTooBig max_size ->
              Printf.fprintf stderr "error: message size limit exceeded. The size payload can at most be %d digits.\n%!" max_size
           | BoundedIO.BadCharacter c ->
              Printf.fprintf stderr "error: bad character: %c\n%!" c
           | BoundedIO.SigPipe ->
              Printf.fprintf stderr "error: broken pipe.\n%!"
           | Json.MalformedJson s ->
              Printf.fprintf stderr "error: malformed JSON: %s\n%!" s
           | Json.AbsentField s ->
              Printf.fprintf stderr "error: expected field %s to be present in message\n%!" s
           | LPMessage.AmbigiousMessage xs ->
              Printf.fprintf stderr "error: message is ambiguous, it contains multiple kinds: %s.\n%!" (string_of_list LPMessage.string_of_kind xs)
           | LPMessage.BadMessage (expected, actual) ->
              Printf.fprintf stderr "error: expected message of kind %s, but the actual message kind is %s.\n%!" (LPMessage.string_of_kind expected) (LPMessage.string_of_kind actual)
           | Resolve.Unresolved s ->
              Printf.fprintf stderr "error: unable to resolve %s\n%!" s
           | Mediator.ConnectionFailure ->
              Printf.fprintf stderr "error: connection failed.\n%!"
           | Mediator.ClientInteractionError (e, pstatus) ->
                let open Proc in
                let msg =
                  match pstatus with
                  | Running -> raise e
                  | Exited _ -> Printf.sprintf "client interaction error: program was not running. Reason: %s." (Proc.string_of_status pstatus)
                in
                Printf.fprintf stderr "error: %s\n%!" msg
           | Mediator.ClientProgramNotFound prog ->
              Printf.fprintf stderr "error: could not locate client program %s.\n%!" prog
           | Mediator.ServerInteractionError _ ->
              Printf.fprintf stderr "error: server interaction error.\n%!"
           | e ->
              Printf.fprintf stderr "fatal error: an unknown exception occurred.\n%!";
             Printexc.print_backtrace Pervasives.stderr; raise e);
           Lwt.fail Exit);
       Unix.close log_fd
     with
       Exit -> Unix.close log_fd; exit 1
     | End_of_file ->
        Printf.fprintf stderr "error: client or server connection was dropped.\n%!"; exit 1
     | Lwt_unix.Timeout ->
        Printf.fprintf stderr "error: client timed out.\n%!"; exit 1

