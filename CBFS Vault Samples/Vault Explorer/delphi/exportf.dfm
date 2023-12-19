object FormExport: TFormExport
  Left = 510
  Top = 543
  BorderStyle = bsDialog
  Caption = 'Export Files By Mask'
  ClientHeight = 110
  ClientWidth = 450
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  DesignSize = (
    450
    110)
  PixelsPerInch = 96
  TextHeight = 13
  object lblDiskPath: TLabel
    Left = 12
    Top = 16
    Width = 26
    Height = 13
    Caption = 'Path:'
  end
  object lblMask: TLabel
    Left = 12
    Top = 48
    Width = 28
    Height = 13
    Caption = 'Mask:'
  end
  object btnOK: TButton
    Left = 286
    Top = 79
    Width = 75
    Height = 23
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object btnCancel: TButton
    Left = 368
    Top = 79
    Width = 75
    Height = 23
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object edtDiskPath: TEdit
    Left = 52
    Top = 12
    Width = 309
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
  end
  object btnBrowse: TButton
    Left = 367
    Top = 12
    Width = 75
    Height = 23
    Anchors = [akTop, akRight]
    Caption = 'Browse...'
    TabOrder = 3
    OnClick = btnBrowseClick
  end
  object chbRecursive: TCheckBox
    Left = 196
    Top = 46
    Width = 185
    Height = 17
    Caption = 'Recurse subfolders'
    Checked = True
    State = cbChecked
    TabOrder = 4
  end
  object edtMask: TEdit
    Left = 52
    Top = 44
    Width = 121
    Height = 21
    TabOrder = 5
    Text = '*.*'
  end
end
