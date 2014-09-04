# The ocp-webedit's Core


## Summary

#### 1. Project's architecture
#### 2. How to do a new widget
#### 3. How to link a new service with the client
#### 4. List of modules


-------------------------


## Project's architecture

We have cut the project in different layer, from the requests manager to 
graphical widgets.

First, the module Request manages to send requests to the server and parse their
responses.
Then, seeing that the server is mainly here to store files, we have set up a
filemanager on the client side too, which must be exactly at the same state of
the server for the current user. This module Filemanager takes the parsed
responses of Request to manage and simulate the user workspace.

We can see that in the website, there are several "parts" or "widgets" like
the sidepanel (a little file explorer) and the tabs (on the editor's top side).
So, if you open a file "toto.ml" you can see it in tabs and sidepanel at the 
same time, and if you rename it, names should have changed on the both side.
Quite simple if you have 2-3 widgets, but with more, it's a huge source of bugs.
That's why we have made a module which centralize all events linked to the 
Filemanager : the Eventmanager.
This Eventmanager will store all Filemanager's important actions like 
create a file as an event. And widgets will be able to save their own action
for a precised event, and will also be able to trigger the event that causes
to call the Filemanager's action and then the list of widgets' actions saved
with this event.

Let's see with an example : we have 3 widgets (A, B and C), and for the
event "open file", A wants to print the file's name, and C wants to print
the file's content. Then A and C will link their print function to this event.
And after 15min, B wants to open a file, so it will trigger the "open file"
event, which will call the open file function of the Filemanager and then
call the A's print function and B's print function (in an arbitrary order).
That's why the function linked must be callbacks.

Moreover, the Filemanager's actions mainly send requests, and results are
handled later (when the request's response is back to the client). So we
need callbacks here too.  But it's just a detail.

Thanks to this Eventmanager, widgets won't depend on an other widget, they
have to be independant of each other.

We can draw the project architecture like this :

```
     ---------------------
     |      SERVER       |
     ---------------------
              /|\
               |
               |
  ===========================
  
   The very "wild" world web
  
  ===========================
              /|\
               |
               |
     ---------------------
     |      Request      |
     ---------------------
              /|\
               |
               |
     ---------------------
     |    Filemanager    |
     --------------------- 
              /|\
               |
               |
     ---------------------
     |   Eventmanager    |
     ---------------------
         /|\   /|\   /|\
          |     |     |__________________________________
          |     |___________________                     |
          |                         |                    |
    -------------------   -------------------   -------------------
    |    Widget  A    |   |    Widget  B    |   |    Widget  C    |
    -------------------   -------------------   -------------------
```


-------------------------


## How to do a new widget

You want to create a new super useless widget ? Let me tell you how to do this !

First let's see in global.mli the "eventlistener" type :

```ocaml
type eventlistener = {
 init : Dom_html.element Js.t -> unit ;
 open_file : file * string -> unit ;
 create_file : file -> unit ;
 ... } ;;
```

This type is everything the widget must "implement" to have a correct and
complete interface with the Eventmanager.  "init" is the widget
function that will be called at the beginning to initialize your
widget in a DOM element, and then the other functions are linked to
Eventmanager's events. After this, you can do whatever you want with
your widget.

Of course, you don't have to implement all the function, that's why there is :

```ocaml
val empty_eventlistener : eventlistener ;;
```

which allow you to do :

```ocaml
let my_widget_listener = { empty_eventlister with
   init = (fun c -> c##innerHTML <- Js.string "hey !") } ;;
````

It's recommanded to always re-define the "init" function.


Then, since you have done your widget's listener, you have to "make it"
thanks to :

```ocaml
Widget.make document##body my_widget_listener;;
```

You have to call this function when you want to build your widget (most of
the time at the beginning in Content.editor_content, called by the main 
project's function).



-------------------------


## How to link a new service with the client


You have already create a new service on the server side and want to use it
on the client side ? Here, I will teach you how to do this.

Take this example : you want to allow user to copy a file in a project to
another project. Then your service expects a file [name], the [path]
where the file is located, and the [dest] path where you have to paste the file.
(We considere there is no verification to do)

See the holy ASCII art's diagram in section 1) from top to bottom.

Let's start with Request.

You have to build the request you have to send with "pull_request" which
take function callbacks for the request success and failure(optionnal), the HTTP method
(mainly "POST"), your service "url" (like "/copy") and the message (which must
be like "arg=val&arg2=val2" in case of POST method).
So it's quite simple to build the message, just be careful with "arg"'s name,
that should be the same on client and server side, otherwise, the request will fail.
The other process to do in your request function is to parse the server 
response. Indeed the pull_request's callbacks takes a string as argument, and
sometimes you will maybe want to parse this string before to call the 
request's function's callback. In our example, we don't mind the server
response. Then, the resquest's function is :

```ocaml
let request_copy_file callback name path dest =
  let msg = Format.sprintf "name=%s&path=%s&dest=%s" name path dest in
  let success str = callback () in
  pull_request ~success ~meth:"POST" ~url:"/copy" ~asyn:true ~msg ()

val request_copy_file : (unit -> unit) -> string -> string -> string -> unit
```

Requests always take "primitive" types, ie you must not use advanced type
of the Filemanager.


Then your Filemanager function.

The copy function needs a file, and a destination project (we don't need the
source project because it is the file.f_project). And it can be useful to
have the new copied file, for the future widgets which need it in their
callback. This Filemanager function have to :
 - convert "advanced" types into "primitive" types for the request
 - modify the filemanager itself to be sure to have the same workspace on client
   and server side
 - call the request with a callback for the future widgets
The signature will be something like this :

```ocaml
val filemanager_copy_file : (file -> unit) -> (file, project) -> unit
```

The first argument is the callback which "send" the new copied file to the
future widgets, and the second one is the required "parameters" to do the
copy action.
And the function itself will look like (note: this is an example, so some
functions/types can have an other name that the real one):

```ocaml
let filemanager_copy_file callback (file, dest) =
    let callback () =
      (* Building of the new copied file thanks to the server response or not *)
      let newfile = intern_create_file dest file.name in
      (* Calling of the widgets' callback with our new file *)
      callback newfile
    in
    (* Preparing request *)
    let name = file.name in
    let path = get_file_location file in
    let dest = get_path dest in
    request_copy_file callback name path dest 
```



After the Filemanager, the whole work is done, we now have to centralize this
action in the Eventmanager.
The Eventmanager needs a ['a, 'b] event with an action which type is
"('a -> unit) -> 'b -> unit". Amazing ! it's matching with our
filemanager_copy_file's signature !
So let's do this :

```ocaml
val event_copy_file : (file, file * project) event

let event_copy_file = new event filemanager_copy_file
```

That's all !



The final step is to incorporate your event in widgets :
Just add your event to the eventlistener type in "global.ml" and "global.mli"
by adding the callback signature.
In our example :

```ocaml
type eventlistener = {
 init : Dom_html.element Js.t -> unit ;
 open_file : file * string -> unit ;
 create_file : file -> unit ;
 copy_file : file -> unit ; (* Here *)
 ... } ;;
```

And don't forget to add the link between the eventlistener and the Eventmanager
in "widget.ml" in the function "make" :

```ocaml
let make c ev =
  ev.init c;
  Eventmanager.open_file#add_event ev.open_file;
  Eventmanager.create_file#add_event ev.create_file;
  Eventmanager.event_copy_file#add_event ev.copy_file; (* Here *)
  ...
```

Then, just add in each widget if necessary, what it will have to do when
the copy event is trigged.






-------------------------


## List of modules

```
          ace : Light bindings for the Ace editor
  centerpanel : Obsolete widget
completion_js : Interface for our library "autocomplete"
       config : Generated by the configuration program, don't touch it
      content : Contains Dom elements of the several site main states
                (loading screen, loginin screen, editor screen)
       dialog : Dialogs with the user like "alert", "confirm", "prompt"
                and "right click" boxes
errors_format : Types for compilation errors parsing
 errors_lexer : Lexer for compilation errors that the compiler give us
errors_report : Add to compilation output's Dom, the link with the eventmanager
                for the goto_next_error event
 eventmanager : The famous Eventmanager, centralize all events and allows
                widgets to be independant of each other
  filemanager : The famous Filemanager, simulate the user workspace and link
                the Request module with the others
       global : Global types and functions (sometimes to avoid circular 
                dependancies)
       indent : Interface for ocp-indent
        login : Loginin module
         main : Program's start point
         menu : Menu widget
    mycompile : Interface for our ocamlc webworker
     myparser : Parser for simple workspace configuration file (contains also
                the famous "split" function !)
   mytoplevel : Interface for our toplevel webworker
      myutils : Somes util functions
      persona : Interface for Persona (authentification service)
      request : Interface between client and server, contains the request
                done to the server and collects server responses
    sidepanel : Light file explorer widget
         tabs : Tab widget
      toolbar : Toolbar widget
    webworker : Light bindings for Javascript's WebWorker
       widget : Implements a widget into the Eventmanager
```
