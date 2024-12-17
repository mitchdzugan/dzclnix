{
  lisp,
  swank,
  writeShellApplication,
}:

writeShellApplication {
  name = "clnix-swank-server";

  text = ''
    # shellcheck source=/dev/null
    source ${swank}/nix-support/setup-hook

    communication_style_expr="(swank-backend::preferred-communication-style)"
    swank_port=4005
    systems_to_load=()

    print_usage() {
      printf 'USAGE: %s [OPTIONS...] <SYSTEMS...>\n\n' "$0"
      printf 'A simple helper to start a Swank server from the command line.\n\n'
      printf 'OPTIONS:\n\n'
      printf '  --communication-style STYLE\n\n'
      printf '      Configures how Lisp reads messages from the editor. Valid values of STYLE\n'
      printf '      are default, nil, fd-handler, sigio, and spawn. For more information\n'
      printf '      about what these values mean, see the SLIME documentation at:\n'
      printf '      https://slime.common-lisp.dev/doc/html/Communication-style.html\n\n'
      printf '  --port PORT\n\n'
      printf '      Set the port number on which Lisp listens for messages from the editor.\n'
      printf '      Defaults to 4005.\n\n'
      printf '  SYSTEMS...\n\n'
      printf '      Any other arguments will be treated as the names of ASDF systems to load\n'
      printf '      before starting the server.\n'
    }
    usage() {
      print_usage >&2
      exit 100
    }

    if [[ "$#" = 0 ]]; then
      usage
    fi
    while [[ "$#" != 0 ]]; do
      case "$1" in
      --communication-style)
        shift
        if [[ "$#" = 0 ]]; then
          usage
        fi
        case "$1" in
        default)
          communication_style_expr="(swank-backend::preferred-communication-style)"
          ;;
        nil)
          communication_style_expr="nil"
          ;;
        fd-handler)
          communication_style_expr=":fd-handler"
          ;;
        sigio)
          communication_style_expr=":sigio"
          ;;
        spawn)
          communication_style_expr=":spawn"
          ;;
        *)
          printf 'invalid communication style\n' >&2
          usage
          ;;
        esac
        ;;
      --port)
        shift
        if [[ "$#" = 0 ]]; then
          usage
        fi
        swank_port="$1"
        ;;
      -*)
        usage
        ;;
      *)
        systems_to_load+=("$1")
        ;;
      esac
      shift
    done

    ${
      lisp.evalCommand [
        "(require :asdf)"
        "(asdf:ensure-source-registry `(:source-registry (:directory ,(uiop:getcwd)) :inherit-configuration))"
        "(asdf:load-system :swank)"
        "(loop for arg in (cddr uiop:*command-line-arguments*) do (asdf:load-system arg))"
        "(setf swank:*communication-style* (eval (read-from-string (first uiop:*command-line-arguments*))))"
        "(swank:create-server :port (parse-integer (second uiop:*command-line-arguments*)) :dont-close t)"
        "(when swank:*communication-style* (format swank/backend:*log-output* \";; Sleeping forever...~%\") (loop (sleep 3600)))"
      ]
    } "$communication_style_expr" "$swank_port" "''${systems_to_load[@]}"
  '';
}
