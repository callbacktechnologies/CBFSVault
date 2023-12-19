object FormImport :TFormImport
  Left = 522
  Top = 473
  BorderStyle = bsDialog
  Caption = 'Import Files By Mask'
  ClientHeight = 248
  ClientWidth = 460
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  DesignSize = (
    460
    248)
  PixelsPerInch = 96
  TextHeight = 13
  object lbDiskPath: TLabel
    Left = 12
    Top = 16
    Width = 26
    Height = 13
    Caption = 'Path:'
  end
  object lbMask: TLabel
    Left = 12
    Top = 48
    Width = 28
    Height = 13
    Caption = 'Mask:'
  end
  object lbPassword: TLabel
    Left = 72
    Top = 108
    Width = 50
    Height = 13
    Caption = '&Password:'
    Enabled = False
    FocusControl = edPassword
  end
  object lbConfirmation: TLabel
    Left = 72
    Top = 136
    Width = 65
    Height = 13
    Caption = '&Confirmation:'
    Enabled = False
    FocusControl = edConfirmation
  end
  object lbCompressionLevel: TLabel
    Left = 72
    Top = 188
    Width = 93
    Height = 13
    Caption = 'Compression Level:'
    Enabled = False
  end
  object lbCompressionLevelValue: TLabel
    Left = 174
    Top = 188
    Width = 35
    Height = 13
    Caption = 'Default'
    Enabled = False
  end
  object btnOK: TButton
    Left = 297
    Top = 219
    Width = 75
    Height = 23
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    Enabled = False
    TabOrder = 0
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 379
    Top = 219
    Width = 75
    Height = 23
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
    OnClick = btnCancelClick
  end
  object edDiskPath: TEdit
    Left = 52
    Top = 12
    Width = 317
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    OnChange = edDiskPathChange
  end
  object edMask: TEdit
    Left = 52
    Top = 44
    Width = 121
    Height = 21
    TabOrder = 3
    Text = '*.*'
  end
  object cbRecursive: TCheckBox
    Left = 196
    Top = 46
    Width = 185
    Height = 17
    Caption = 'Recurse subfolders'
    Checked = True
    State = cbChecked
    TabOrder = 4
  end
  object btnBrowse: TButton
    Left = 379
    Top = 11
    Width = 75
    Height = 23
    Anchors = [akTop, akRight]
    Caption = 'Browse...'
    TabOrder = 5
    OnClick = btnBrowseClick
  end
  object cbEncryption: TCheckBox
    Left = 52
    Top = 80
    Width = 321
    Height = 17
    Caption = '&Encrypt files with password (using AES256 encryption)'
    TabOrder = 6
    OnClick = cbEncryptionClick
  end
  object edPassword: TEdit
    Left = 152
    Top = 104
    Width = 217
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Enabled = False
    PasswordChar = '*'
    TabOrder = 7
  end
  object edConfirmation: TEdit
    Left = 152
    Top = 132
    Width = 217
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Enabled = False
    PasswordChar = '*'
    TabOrder = 8
  end
  object cbCompress: TCheckBox
    Left = 52
    Top = 160
    Width = 321
    Height = 17
    Caption = '&Compress files'
    TabOrder = 9
    OnClick = cbCompressClick
  end
  object tbCompressionLevel: TTrackBar
    Left = 232
    Top = 174
    Width = 145
    Height = 31
    Anchors = [akLeft, akTop, akRight]
    Enabled = False
    Max = 9
    PageSize = 1
    TabOrder = 10
    ThumbLength = 18
    TickMarks = tmTopLeft
    OnChange = tbCompressionLevelChange
  end
end
