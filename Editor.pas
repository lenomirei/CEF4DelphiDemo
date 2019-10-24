unit Editor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ToolWin, ComCtrls, ExtCtrls, uCEFChromium,
  uCEFWinControl, uCEFWindowParent, uCEFInterfaces, ExtDlgs, Displayer, uCEFTypes;

type
  TEditForm = class(TForm)
    ToolBar1: TToolBar;
    SendButton: TButton;
    ImageButton: TButton;
    CEFWindowParent1: TCEFWindowParent;
    Chromium1: TChromium;
    Timer1: TTimer;
    ImageOpenDialog: TOpenDialog;
    procedure SendButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Chromium1LoadEnd(Sender: TObject; const browser: ICefBrowser;
      const frame: ICefFrame; httpStatusCode: Integer);
    procedure Chromium1AfterCreated(Sender: TObject;
      const browser: ICefBrowser);
    procedure ImageButtonClick(Sender: TObject);
    procedure Chromium1TextResultAvailable(Sender: TObject;
      const aText: ustring);

  private
    { Private declarations }
    FCanClose : boolean;
    FClosing : boolean;
    FDisplayForm : TDisplayForm;
    procedure EnableDesignMode();
    procedure InsertImage(ImagePath : string);

  public
    { Public declarations }
  end;

var
  EditForm: TEditForm;

implementation

{$R *.dfm}

procedure TEditForm.SendButtonClick(Sender: TObject);
begin
  // Open another form to preview
  Chromium1.RetrieveHTML();
end;

procedure TEditForm.EnableDesignMode();
var
  TempCode : string;
begin
  TempCode := 'document.designMode = "on";';

  Chromium1.ExecuteJavaScript(TempCode, 'about:blank');  
end;

procedure TEditForm.FormCreate(Sender: TObject);
begin
  FCanClose := false;
  FClosing := false;
  Chromium1.DefaultUrl := 'file:\\\DefaultMailTemplate.html';
end;



procedure TEditForm.FormShow(Sender: TObject);
begin
  if not(Chromium1.CreateBrowser(CEFWindowParent1)) then
    Timer1.Enabled := true;
end;

procedure TEditForm.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := false;
  if not(Chromium1.CreateBrowser(CEFWindowParent1)) and not(Chromium1.Initialized) then
    Timer1.Enabled := true;
end;

procedure TEditForm.Chromium1LoadEnd(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame;
  httpStatusCode: Integer);
begin
  if (frame <> nil) and not(frame.isMain) then exit;

  EnableDesignMode();
end;

procedure TEditForm.Chromium1AfterCreated(Sender: TObject;
  const browser: ICefBrowser);
begin
  Caption := 'browser';
end;

procedure TEditForm.ImageButtonClick(Sender: TObject);
begin
  if ImageOpenDialog.Execute then
    InsertImage(ImageOpenDialog.FileName);
end;

procedure TEditForm.InsertImage(ImagePath : string);
var
  TempCode : string;
begin
  TempCode := 'document.execCommand("insertImage", false, "' + ImagePath + '");';
  TempCode := StringReplace(TempCode, '\', '/', [rfReplaceAll]);
  Chromium1.ExecuteJavaScript(TempCode, 'about:blank');
end;

procedure TEditForm.Chromium1TextResultAvailable(Sender: TObject;
  const aText: ustring);
var
  TempLines : TStringList;
  TempFilePath : string;
begin
  // Save a temp file here
  TempLines := nil;
  TempFilePath := ExtractFilePath(Application.ExeName);
  try
    TempLines := TStringList.Create;
    TempLines.Text := aText;
    TempLines.SaveToFile(TempFilePath + 'temp.html');
  finally
    if(TempLines <> nil) then FreeAndNil(TempLines);
  end;
  // Open display form here
  FDisplayForm := TDisplayForm.Create(Self, aText);
  FDisplayForm.Show();
end;

end.
