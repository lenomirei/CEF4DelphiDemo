program HTMLEditor;

uses
  Forms,
  Editor in 'Editor.pas' {TEditForm},
  Displayer in 'Displayer.pas' {TDisplayForm},
  Windows,
  uCEFApplication;

{$R *.res}

begin
  GlobalCEFApp := TCefApplication.Create;
  if GlobalCEFApp.StartMainProcess then
    begin
      Application.Initialize;
      Application.CreateForm(TEditForm, EditForm);
      Application.Run;
    end;

  GlobalCEFApp.Free;
end.
