object EditForm: TEditForm
  Left = 1631
  Top = 369
  Width = 851
  Height = 679
  Caption = #26410#21629#21517' - '#20889#37038#20214
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
    Width = 835
    Height = 41
    Caption = 'ToolBar1'
    TabOrder = 0
    object SendButton: TButton
      Left = 0
      Top = 2
      Width = 75
      Height = 22
      Caption = 'Send'
      TabOrder = 0
      OnClick = SendButtonClick
    end
    object ImageButton: TButton
      Left = 75
      Top = 2
      Width = 75
      Height = 22
      Caption = 'Image'
      TabOrder = 1
      OnClick = ImageButtonClick
    end
    object SaveButton: TButton
      Left = 150
      Top = 2
      Width = 75
      Height = 22
      Caption = 'Save'
      TabOrder = 2
      OnClick = SaveButtonClick
    end
  end
  object CEFWindowParent1: TCEFWindowParent
    Left = 0
    Top = 41
    Width = 835
    Height = 599
    Align = alClient
    TabOrder = 1
    DoubleBuffered = False
  end
  object Chromium1: TChromium
    OnTextResultAvailable = Chromium1TextResultAvailable
    OnLoadEnd = Chromium1LoadEnd
    OnAfterCreated = Chromium1AfterCreated
    Left = 40
    Top = 128
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 40
    Top = 88
  end
  object ImageOpenDialog: TOpenDialog
    Left = 96
    Top = 88
  end
  object SaveDialog1: TSaveDialog
    Left = 96
    Top = 128
  end
end
