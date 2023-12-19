object FormImportfile :TFormImportfile
  Left = 576
  Top = 435
  BorderStyle = bsDialog
  Caption = 'Creating file'
  ClientHeight = 232
  ClientWidth = 336
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  DesignSize = (
    336
    232)
  PixelsPerInch = 96
  TextHeight = 13
  object lbText: TLabel
    Left = 12
    Top = 12
    Width = 305
    Height = 37
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'Select properties for creating file %s'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object lbPassword: TLabel
    Left = 32
    Top = 84
    Width = 50
    Height = 13
    Caption = '&Password:'
    Enabled = False
    FocusControl = edPassword
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lbConfirmation: TLabel
    Left = 32
    Top = 112
    Width = 65
    Height = 13
    Caption = '&Confirmation:'
    Enabled = False
    FocusControl = edConfirmation
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lbCompressionLevel: TLabel
    Left = 32
    Top = 168
    Width = 93
    Height = 13
    Caption = 'Compression Level:'
    Enabled = False
  end
  object lbCompressionLevelValue: TLabel
    Left = 134
    Top = 168
    Width = 35
    Height = 13
    Caption = 'Default'
    Enabled = False
  end
  object btnOK: TButton
    Left = 168
    Top = 202
    Width = 75
    Height = 23
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 0
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 250
    Top = 202
    Width = 75
    Height = 23
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
    OnClick = btnCancelClick
  end
  object btnAll: TButton
    Left = 88
    Top = 202
    Width = 75
    Height = 23
    Anchors = [akRight, akBottom]
    Caption = 'All'
    TabOrder = 2
    Visible = False
    OnClick = btnAllClick
  end
  object cbEncryption: TCheckBox
    Left = 12
    Top = 56
    Width = 305
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = '&Encrypt file with password'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    OnClick = cbEncryptionClick
  end
  object edPassword: TEdit
    Left = 112
    Top = 80
    Width = 212
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Enabled = False
    PasswordChar = '*'
    TabOrder = 4
  end
  object edConfirmation: TEdit
    Left = 112
    Top = 108
    Width = 212
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Enabled = False
    PasswordChar = '*'
    TabOrder = 5
  end
  object cbCompress: TCheckBox
    Left = 12
    Top = 140
    Width = 305
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = '&Compress file'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 6
    OnClick = cbCompressClick
  end
  object tbCompressionLevel: TTrackBar
    Left = 184
    Top = 154
    Width = 145
    Height = 31
    Enabled = False
    Max = 9
    PageSize = 1
    TabOrder = 7
    ThumbLength = 18
    TickMarks = tmTopLeft
    OnChange = tbCompressionLevelChange
  end
end
