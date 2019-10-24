program HTMLEditor;

uses
  Forms,
  Editor in 'Editor.pas' {TEditForm},
  Displayer in 'Displayer.pas' {TDisplayForm},
  Windows,
  uCEFApplication;

{$R *.res}

begin
  GlobalCEFApp                      := TCefApplication.Create;
  GlobalCEFApp.DisableFeatures      := 'NetworkService';
  if GlobalCEFApp.StartMainProcess then
    begin
      Application.Initialize;
      Application.CreateForm(TEditForm, EditForm);
      Application.Run;
    end;

  DestroyGlobalCEFApp;
end.
