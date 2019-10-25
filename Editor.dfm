object EditForm: TEditForm
  Left = 1481
  Top = 547
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
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 835
    Height = 25
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
    object SaveButton: TButton
      Left = 75
      Top = 2
      Width = 75
      Height = 22
      Caption = 'Save'
      TabOrder = 2
    end
    object ImageButton: TButton
      Left = 150
      Top = 2
      Width = 75
      Height = 22
      Caption = 'Image'
      TabOrder = 1
      OnClick = ImageButtonClick
    end
  end
  object CEFWindowParent1: TCEFWindowParent
    Left = 0
    Top = 54
    Width = 835
    Height = 586
    Align = alClient
    TabOrder = 1
    DoubleBuffered = False
  end
  object ToolBar2: TToolBar
    Left = 0
    Top = 25
    Width = 835
    Height = 29
    Caption = 'ToolBar2'
    TabOrder = 2
    object FontColorButton: TButton
      Left = 0
      Top = 2
      Width = 75
      Height = 22
      Caption = #39068#33394
      TabOrder = 0
      OnClick = FontColorButtonClick
    end
    object FontBackgroundColorButton: TButton
      Left = 75
      Top = 2
      Width = 75
      Height = 22
      Caption = #32972#26223#33394
      TabOrder = 1
      OnClick = FontBackgroundColorButtonClick
    end
    object BoldButton: TButton
      Left = 150
      Top = 2
      Width = 75
      Height = 22
      Caption = #21152#31895
      TabOrder = 2
      OnClick = BoldButtonClick
    end
    object ItalicButton: TButton
      Left = 225
      Top = 2
      Width = 75
      Height = 22
      Caption = #26012#20307
      TabOrder = 3
      OnClick = ItalicButtonClick
    end
    object UnderlineButton: TButton
      Left = 300
      Top = 2
      Width = 75
      Height = 22
      Caption = #19979#21010#32447
      TabOrder = 4
      OnClick = UnderlineButtonClick
    end
    object StrikethroughButton: TButton
      Left = 375
      Top = 2
      Width = 75
      Height = 22
      Caption = #21024#38500#32447
      TabOrder = 5
      OnClick = StrikethroughButtonClick
    end
    object UnorderedListButton: TButton
      Left = 450
      Top = 2
      Width = 71
      Height = 22
      Caption = #26080#24207#21015#34920
      TabOrder = 6
      OnClick = UnorderedListButtonClick
    end
    object OrderedListButton: TButton
      Left = 521
      Top = 2
      Width = 75
      Height = 22
      Caption = #26377#24207#21015#34920
      TabOrder = 7
      OnClick = OrderedListButtonClick
    end
  end
  object Chromium1: TChromium
    OnTextResultAvailable = Chromium1TextResultAvailable
    OnLoadEnd = Chromium1LoadEnd
    OnAfterCreated = Chromium1AfterCreated
    OnBeforeClose = Chromium1BeforeClose
    OnClose = Chromium1Close
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
  object ColorDialog: TColorDialog
    Left = 96
    Top = 128
  end
end
