open Cohttp_lwt_unix

let default_port = 8421

let html =
  {|
<!DOCTYPE html>
<html lang="en">
    <head>
  <script src="https://cdn.jsdelivr.net/npm/vega@5"></script>
  <script src="https://cdn.jsdelivr.net/npm/vega-lite@5"></script>
  <script src="https://cdn.jsdelivr.net/npm/vega-embed@6"></script>
      <script type="text/javascript" src="main.js"></script>
<style>
form.vega-bindings {
    display: flex;
    position: absolute;
    top: 0px;
    flex-direction:column;
}
div#viz {
    border-style: "solid"
}
.olirvu-container {
    display: flex;
}
</style>
    </head>
    <body>
      <h1>Visualizations Playground</h1>
      <div class="olirvu-container">
        <div id="app"></div>
        <div id="viz"></div>
      </div>
    </body>
</html>
|}
;;

let respond_string ~content_type ~status ~body ?headers =
  let headers = Cohttp.Header.add_opt headers "Content-Type" content_type in
  Server.respond_string ~headers ~status ~body
;;

let respond_file ~content_type ?headers s =
  let headers = Cohttp.Header.add_opt headers "Content-Type" content_type in
  Server.respond_file ~headers ~fname:s ()
;;

let json_pattern = Base.String.Search_pattern.create ~case_sensitive:false "json"

let server ~port =
  let callback _conn req _body =
    let uri = req |> Request.uri |> Uri.path in
    match uri with
    | "" | "/" | "/index.html" ->
      respond_string ~content_type:"text/html" ~status:`OK ~body:html ()
    | "/main.js" ->
      respond_string
        ~content_type:"application/javascript"
        ~status:`OK
        ~body:Embedded_files.main_dot_bc_dot_js
        ()
    | uri when Base.String.Search_pattern.matches json_pattern uri ->
      respond_file ~content_type:"application/json" (Base.String.drop_prefix uri 1)
    | _ -> respond_string ~content_type:"text/html" ~status:`Not_found ~body:"" ()
  in
  Server.create ~mode:(`TCP (`Port port)) (Server.make ~callback ())
;;

let main ~port =
  Stdio.printf "\nRunning at: http://localhost:%d/index.html\n" port;
  Stdio.Out_channel.flush Stdio.stdout;
  Lwt_main.run @@ server ~port
;;

let () = main ~port:default_port
