object DisplayForm: TDisplayForm
  Left = 926
  Top = 559
  Width = 857
  Height = 650
  Caption = #21021#22987#21270#20013'......'#35831#31245#31561
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ChromiumWindow: TChromiumWindow
    Left = 0
    Top = 0
    Width = 841
    Height = 611
    Align = alClient
    TabOrder = 0
    DoubleBuffered = False
    OnBeforeClose = ChromiumWindowBeforeClose
    OnAfterCreated = ChromiumWindowAfterCreated
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 64
    Top = 72
  end
end
