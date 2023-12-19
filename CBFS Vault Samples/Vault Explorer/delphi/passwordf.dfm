object FormPassword: TFormPassword
  Left = 589
  Top = 588
  BorderStyle = bsDialog
  Caption = 'Enter Password'
  ClientHeight = 109
  ClientWidth = 425
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lbPrompt: TLabel
    Left = 12
    Top = 12
    Width = 405
    Height = 26
    AutoSize = False
    Caption = 'Password required to access %s %s'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    WordWrap = True
  end
  object edPassword: TEdit
    Left = 12
    Top = 44
    Width = 405
    Height = 21
    PasswordChar = '*'
    TabOrder = 0
  end
  object btnOK: TButton
    Left = 261
    Top = 79
    Width = 75
    Height = 23
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 343
    Top = 79
    Width = 75
    Height = 23
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
