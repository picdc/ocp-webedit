
open Global

let make c ev =
  ev.init c;
  Eventmanager.open_workspace#add_event ev.open_workspace;
  Eventmanager.open_file#add_event ev.open_file;
  Eventmanager.close_workspace#add_event ev.close_workspace;
  Eventmanager.close_file#add_event ev.close_file;
  Eventmanager.create_file#add_event ev.create_file;
  Eventmanager.create_project#add_event ev.create_project;
  Eventmanager.create_directory#add_event ev.create_directory;
  Eventmanager.rename_file#add_event ev.rename_file;
  Eventmanager.rename_project#add_event ev.rename_project;
  Eventmanager.rename_directory#add_event ev.rename_directory;
  Eventmanager.delete_file#add_event ev.delete_file;
  Eventmanager.delete_project#add_event ev.delete_project;
  Eventmanager.delete_directory#add_event ev.delete_directory;
  Eventmanager.save_file#add_event ev.save_file;
  Eventmanager.save_conf#add_event ev.save_conf;
  Eventmanager.unsave_file#add_event ev.unsaved_file;
  Eventmanager.import_file#add_event ev.import_file;
  Eventmanager.switch_file#add_event ev.switch_file;
  Eventmanager.link_after#add_event ev.link_after;
  Eventmanager.link_before#add_event ev.link_before;
  Eventmanager.goto_next_error#add_event ev.goto_next_error;
  Eventmanager.compile#add_event ev.compile;
  Eventmanager.import_project#add_event ev.import_project;
  Eventmanager.import_library#add_event ev.import_library
