object DisplayForm: TDisplayForm
  Left = 1364
  Top = 453
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
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 841
    Height = 41
    Caption = 'ToolBar1'
    TabOrder = 0
    object Button1: TButton
      Left = 0
      Top = 2
      Width = 75
      Height = 22
      Caption = 'Button1'
      TabOrder = 0
    end
    object Button2: TButton
      Left = 75
      Top = 2
      Width = 75
      Height = 22
      Caption = 'Button2'
      TabOrder = 1
    end
  end
  object ChromiumWindow: TChromiumWindow
    Left = 0
    Top = 41
    Width = 841
    Height = 570
    Align = alClient
    TabOrder = 1
    DoubleBuffered = False
    OnAfterCreated = ChromiumWindowAfterCreated
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 64
    Top = 72
  end
end
