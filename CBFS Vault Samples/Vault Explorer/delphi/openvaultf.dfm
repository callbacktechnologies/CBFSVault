object FormOpenvault :TFormOpenvault
  Left = 197
  Top = 106
  BorderStyle = bsDialog
  Caption = 'Open Vault'
  ClientHeight = 186
  ClientWidth = 408
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  DesignSize = (
    408
    186)
  PixelsPerInch = 96
  TextHeight = 13
  object lbVaultFileName: TLabel
    Left = 16
    Top = 16
    Width = 90
    Height = 13
    Caption = 'Vault File Name:'
  end
  object lbOpenMode: TLabel
    Left = 16
    Top = 72
    Width = 59
    Height = 13
    Caption = 'Open Mode:'
  end
  object btnOK: TButton
    Left = 245
    Top = 155
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object btnCancel: TButton
    Left = 325
    Top = 155
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object edVaultFileName: TEdit
    Left = 16
    Top = 32
    Width = 301
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
  end
  object cbReadOnly: TCheckBox
    Left = 16
    Top = 96
    Width = 177
    Height = 17
    Hint = 'Specifies if the vault must be opened-in readonly mode.'
    Caption = 'Read Only'
    TabOrder = 3
    OnClick = cbReadOnlyClick
  end
  object cbUseJournaling: TCheckBox
    Left = 16
    Top = 120
    Width = 177
    Height = 17
    Hint = 
      'Enables transactions and journal. Note, that transactions slow d' +
      'own file write operations significantly.'
    Caption = 'Use Transactions'
    TabOrder = 4
  end
  object cbAutoCompact: TCheckBox
    Left = 200
    Top = 96
    Width = 113
    Height = 17
    Hint = 
      'If percentage of free space in vault becomes higher then value' +
      ' of AutoCompact then vault will be compacted automatically.'
    Caption = 'Auto Compact'
    Checked = True
    State = cbChecked
    TabOrder = 5
    OnClick = cbAutoCompactClick
  end
  object cbUseAccessTime: TCheckBox
    Left = 200
    Top = 120
    Width = 169
    Height = 17
    Hint = 
      'Defines if the last access time is written to vault when the f' +
      'ile/folder is accessed. Writing access time slows down the opera' +
      'tions.'
    Caption = 'Use Access Time'
    TabOrder = 6
  end
  object edAutoCompact: TEdit
    Left = 316
    Top = 94
    Width = 45
    Height = 21
    TabOrder = 7
    Text = '25'
  end
  object spnAutoCompact: TUpDown
    Left = 361
    Top = 94
    Width = 15
    Height = 21
    Associate = edAutoCompact
    Min = 1
    Position = 25
    TabOrder = 8
    Thousands = False
  end
  object btnBrowse: TButton
    Left = 325
    Top = 30
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Browse...'
    TabOrder = 9
    OnClick = btnBrowseClick
  end
  object OpenDlg: TOpenDialog
    DefaultExt = '.svlt'
    Filter = 'CBFS Vault (*.svlt)|*.svlt|Any File (*.*)|*.*'
    Options = [ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 148
    Top = 20
  end
end
