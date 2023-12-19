object FormVaultencryption :TFormVaultencryption
  Left = 576
  Top = 435
  BorderStyle = bsDialog
  Caption = 'Advanced Vault Properties'
  ClientHeight = 176
  ClientWidth = 351
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  DesignSize = (
    351
    176)
  PixelsPerInch = 96
  TextHeight = 13
  object lbText: TLabel
    Left = 12
    Top = 12
    Width = 329
    Height = 37
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'Select properties for vault %s'
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
  object btnOK: TButton
    Left = 183
    Top = 146
    Width = 75
    Height = 23
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 0
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 265
    Top = 146
    Width = 75
    Height = 23
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
    OnClick = btnCancelClick
  end
  object cbEncryption: TCheckBox
    Left = 12
    Top = 56
    Width = 329
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = '&Encrypt file with password'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    OnClick = cbEncryptionClick
  end
  object edPassword: TEdit
    Left = 112
    Top = 80
    Width = 229
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Enabled = False
    PasswordChar = '*'
    TabOrder = 3
  end
  object edConfirmation: TEdit
    Left = 112
    Top = 108
    Width = 229
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Enabled = False
    PasswordChar = '*'
    TabOrder = 4
  end
end
