unit Displayer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ToolWin, ComCtrls, uCEFWinControl, uCEFChromiumWindow, uCEFTypes,
  uCEFChromium, uCEFWindowParent, ExtCtrls;

type
  TDisplayForm = class(TForm)
    ToolBar1: TToolBar;
    Button1: TButton;
    Button2: TButton;
    ChromiumWindow: TChromiumWindow;
    Timer1: TTimer;
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ChromiumWindowAfterCreated(Sender: TObject);
    procedure FormCreate(Sender: TObject);

  private
    { Private declarations }
    FHTMLSource : ustring;
    FCanClose : boolean;
    FClosing : boolean;
  public
    { Public declarations }
    constructor Create(AOwner : TComponent; HTMLSource : ustring);
  end;

var
  DisplayForm: TDisplayForm;

implementation

{$R *.dfm}

constructor TDisplayForm.Create(AOwner : TComponent; HTMLSource : ustring);
begin
  inherited Create(AOwner);
  FHTMLSource := HTMLSource;
end;



procedure TDisplayForm.FormShow(Sender: TObject);
begin
  if not(ChromiumWindow.CreateBrowser()) then
    Timer1.Enabled := true;
end;

procedure TDisplayForm.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := false;
  if not(ChromiumWindow.CreateBrowser()) and not(ChromiumWindow.Initialized) then
    Timer1.Enabled := true;
end;

procedure TDisplayForm.ChromiumWindowAfterCreated(Sender: TObject);
var
TempFilePath : string;
begin
  Caption := '≤‚ ‘” º˛';
  TempFilePath := ExtractFilePath(Application.ExeName);
  ChromiumWindow.ChromiumBrowser.LoadUrl(TempFilePath + 'temp.html');
end;



procedure TDisplayForm.FormCreate(Sender: TObject);
begin
  FCanClose := false;
  FClosing := false;

end;

end.
