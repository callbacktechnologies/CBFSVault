object FormLink: TFormLink
  Left = 343
  Top = 276
  BorderStyle = bsDialog
  Caption = 'Create Link'
  ClientHeight = 151
  ClientWidth = 368
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  DesignSize = (
    368
    151)
  PixelsPerInch = 96
  TextHeight = 13
  object lbLinkName: TLabel
    Left = 8
    Top = 12
    Width = 54
    Height = 13
    Caption = 'Link Name:'
  end
  object lbDestPath: TLabel
    Left = 8
    Top = 60
    Width = 80
    Height = 13
    Caption = 'Destination path:'
  end
  object btnOK: TButton
    Left = 207
    Top = 119
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object btnCancel: TButton
    Left = 287
    Top = 119
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object edLinkName: TEdit
    Left = 8
    Top = 28
    Width = 273
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
  end
  object btnBrowse: TButton
    Left = 287
    Top = 27
    Width = 75
    Height = 23
    Anchors = [akTop, akRight]
    Caption = 'Browse...'
    TabOrder = 3
    OnClick = btnBrowseClick
  end
  object btnDestBrowse: TButton
    Left = 287
    Top = 75
    Width = 75
    Height = 23
    Anchors = [akTop, akRight]
    Caption = 'Browse...'
    TabOrder = 4
    OnClick = btnDestBrowseClick
  end
  object edDestPath: TEdit
    Left = 8
    Top = 76
    Width = 273
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 5
  end
end
