object FormWizard: TFormWizard
  Left = 418
  Top = 287
  BorderStyle = bsDialog
  Caption = 'New Vault Wizard'
  ClientHeight = 306
  ClientWidth = 465
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlStep4: TPanel
    Left = 0
    Top = 0
    Width = 465
    Height = 258
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 5
    Visible = False
    object bvlStep4Top: TBevel
      Left = 0
      Top = 57
      Width = 465
      Height = 4
      Align = alTop
      Shape = bsTopLine
    end
    object lblStep4Prompt: TLabel
      Left = 8
      Top = 72
      Width = 27
      Height = 13
      Caption = 'Logo:'
    end
    object pnlStep4Top: TPanel
      Left = 0
      Top = 0
      Width = 465
      Height = 57
      Align = alTop
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 0
      object lblStep4Description: TLabel
        Left = 24
        Top = 32
        Width = 141
        Height = 13
        Caption = 'Enter a logo for new vault '
      end
      object lblStep4Caption: TLabel
        Left = 12
        Top = 10
        Width = 87
        Height = 16
        Caption = 'Vault Logo'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
    end
    object mmLogo: TMemo
      Left = 8
      Top = 96
      Width = 449
      Height = 153
      Lines.Strings = (
        'CBFS Vault for Windows')
      TabOrder = 1
    end
  end
  object pnlStep5: TPanel
    Left = 0
    Top = 0
    Width = 465
    Height = 258
    Align = alClient
    BevelOuter = bvNone
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
    Visible = False
    OnEnter = pnlStep5Enter
    object bvlStep5Top: TBevel
      Left = 0
      Top = 57
      Width = 465
      Height = 4
      Align = alTop
      Shape = bsTopLine
    end
    object lblStep5Prompt: TLabel
      Left = 12
      Top = 72
      Width = 49
      Height = 13
      Caption = 'File name:'
    end
    object pnlStep5Top: TPanel
      Left = 0
      Top = 0
      Width = 465
      Height = 57
      Align = alTop
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 0
      object lblStep5Caption: TLabel
        Left = 12
        Top = 10
        Width = 115
        Height = 16
        Caption = 'Vault File Name'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lblStep5Description: TLabel
        Left = 24
        Top = 32
        Width = 155
        Height = 13
        Caption = 'Select file name for new vault'
      end
    end
    object edtFileName: TEdit
      Left = 24
      Top = 92
      Width = 341
      Height = 21
      TabOrder = 1
      OnChange = edtFileNameChange
    end
    object btnBrowse: TButton
      Left = 376
      Top = 91
      Width = 75
      Height = 23
      Caption = 'Browse...'
      TabOrder = 2
      OnClick = btnBrowseClick
    end
  end
  object pnlStep6: TPanel
    Left = 0
    Top = 0
    Width = 465
    Height = 258
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 6
    Visible = False
    object bvlStep6Top: TBevel
      Left = 0
      Top = 57
      Width = 465
      Height = 4
      Align = alTop
      Shape = bsTopLine
    end
    object pnlStep6Top: TPanel
      Left = 0
      Top = 0
      Width = 465
      Height = 57
      Align = alTop
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 0
      object lblStep6Caption: TLabel
        Left = 12
        Top = 10
        Width = 101
        Height = 16
        Caption = 'lblStep5Caption'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lblStep6Description: TLabel
        Left = 24
        Top = 32
        Width = 91
        Height = 13
        Caption = 'lblStep5Description'
      end
    end
  end
  object pnlStep1: TPanel
    Left = 0
    Top = 0
    Width = 465
    Height = 258
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    Visible = False
    OnEnter = pnlStep1Enter
    object bvlStep1Top: TBevel
      Left = 0
      Top = 57
      Width = 465
      Height = 4
      Align = alTop
      Shape = bsTopLine
    end
    object lblStep1Prompt: TLabel
      Left = 12
      Top = 72
      Width = 188
      Height = 13
      Caption = 'Please specify size of the new vault:'
    end
    object lblVariableSize: TLabel
      Left = 40
      Top = 124
      Width = 385
      Height = 33
      AutoSize = False
      Caption = 
        'Vault will increase and decrese in size automatically to fit' +
        ' all containing directories and files'
      WordWrap = True
    end
    object lblFixedSize: TLabel
      Left = 40
      Top = 184
      Width = 385
      Height = 33
      AutoSize = False
      Caption = 
        'Vault will be created with specified size and can'#39't be increas' +
        'ed or decreased in the future'
      WordWrap = True
    end
    object lblVaultSize: TLabel
      Left = 40
      Top = 224
      Width = 124
      Height = 13
      Caption = 'Size of new vault (MB):'
      Enabled = False
    end
    object pnlStep1Top: TPanel
      Left = 0
      Top = 0
      Width = 465
      Height = 57
      Align = alTop
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 0
      object lblStep1Caption: TLabel
        Left = 12
        Top = 10
        Width = 82
        Height = 16
        Caption = 'Vault Size'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lblStep1Description: TLabel
        Left = 24
        Top = 32
        Width = 246
        Height = 13
        Caption = 'Chose beetwen variable size and fixed size vault'
      end
    end
    object rbtVariableSize: TRadioButton
      Left = 24
      Top = 100
      Width = 409
      Height = 17
      Caption = '&Variable Size'
      Checked = True
      TabOrder = 1
      TabStop = True
      OnClick = rbtFixedSizeClick
    end
    object rbtFixedSize: TRadioButton
      Left = 24
      Top = 160
      Width = 409
      Height = 17
      Caption = '&Fixed Size'
      TabOrder = 2
      OnClick = rbtFixedSizeClick
    end
    object edtVaultSize: TEdit
      Left = 172
      Top = 222
      Width = 45
      Height = 21
      Enabled = False
      TabOrder = 3
      Text = '10'
    end
    object spnVaultSize: TUpDown
      Left = 217
      Top = 222
      Width = 16
      Height = 21
      Associate = edtVaultSize
      Enabled = False
      Min = 1
      Max = 32767
      Position = 10
      TabOrder = 4
      Thousands = False
    end
  end
  object pnlStep2: TPanel
    Left = 0
    Top = 0
    Width = 465
    Height = 258
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 3
    Visible = False
    OnEnter = pnlStep2Enter
    OnExit = pnlStep2Exit
    object bvlStep2Top: TBevel
      Left = 0
      Top = 57
      Width = 465
      Height = 4
      Align = alTop
      Shape = bsTopLine
    end
    object lblStep2Prompt: TLabel
      Left = 12
      Top = 72
      Width = 441
      Height = 17
      AutoSize = False
      Caption = 
        'You can encrypt all information in vault or select encryption ' +
        'for each file separately'#13#10
      WordWrap = True
    end
    object lblPassword: TLabel
      Left = 44
      Top = 120
      Width = 50
      Height = 13
      Caption = '&Password:'
      Enabled = False
      FocusControl = edtPassword
    end
    object lblConfirmation: TLabel
      Left = 44
      Top = 148
      Width = 65
      Height = 13
      Caption = '&Confirmation:'
      Enabled = False
      FocusControl = edtConfirmation
    end
    object pnlStep2Top: TPanel
      Left = 0
      Top = 0
      Width = 465
      Height = 57
      Align = alTop
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 0
      object lblStep2Caption: TLabel
        Left = 12
        Top = 10
        Width = 125
        Height = 16
        Caption = 'Vault Encryption'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lblStep2Description: TLabel
        Left = 24
        Top = 32
        Width = 176
        Height = 13
        Caption = 'Set encyption of new vault, if any'
      end
    end
    object chbEncryption: TCheckBox
      Left = 24
      Top = 92
      Width = 385
      Height = 17
      Caption = '&Encrypt whole vault with password (using AES256 algorithm)'
      TabOrder = 1
      OnClick = chbEncryptionClick
    end
    object edtPassword: TEdit
      Left = 124
      Top = 116
      Width = 221
      Height = 21
      Enabled = False
      PasswordChar = '*'
      TabOrder = 2
    end
    object edtConfirmation: TEdit
      Left = 124
      Top = 144
      Width = 221
      Height = 21
      Enabled = False
      PasswordChar = '*'
      TabOrder = 3
    end
  end
  object pnlStep3: TPanel
    Left = 0
    Top = 0
    Width = 465
    Height = 258
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    Visible = False
    OnEnter = pnlStep3Enter
    OnExit = pnlStep3Exit
    object bvlStep3Top: TBevel
      Left = 0
      Top = 57
      Width = 465
      Height = 4
      Align = alTop
      Shape = bsTopLine
    end
    object lblPageSize: TLabel
      Left = 12
      Top = 72
      Width = 441
      Height = 33
      AutoSize = False
      Caption = 
        'CBFS Vault writes, reads and processes data by pages. Correct ' +
        'page size can increase efficiency of operations. If your vault' +
        ' contains large files, increase page size.'
      WordWrap = True
    end
    object lblPageSizeValue: TLabel
      Left = 40
      Top = 112
      Width = 49
      Height = 13
      Caption = 'Page &size:'
      FocusControl = cmbPageSize
    end
    object lblPartSize: TLabel
      Left = 40
      Top = 168
      Width = 104
      Height = 13
      Caption = 'Size of each file (MB):'
      Enabled = False
    end
    object pnlStep3Top: TPanel
      Left = 0
      Top = 0
      Width = 465
      Height = 57
      Align = alTop
      BevelOuter = bvNone
      Color = clWindow
      TabOrder = 0
      object lblStep3Caption: TLabel
        Left = 12
        Top = 10
        Width = 124
        Height = 16
        Caption = 'Vault Properties'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lblStep3Description: TLabel
        Left = 24
        Top = 32
        Width = 144
        Height = 13
        Caption = 'Set properties of new vault'
      end
    end
    object cmbPageSize: TComboBox
      Left = 120
      Top = 108
      Width = 113
      Height = 21
      Style = csDropDownList
      DropDownCount = 9
      TabOrder = 1
      OnChange = cmbPageSizeChange
      Items.Strings = (
        '256 bytes'
        '512 bytes'
        '1024 bytes'
        '2048 bytes'
        '4096 bytes'
        '8192 bytes'
        '16384 bytes'
        '32768 bytes'
        '65536 bytes')
    end
    object chbParted: TCheckBox
      Left = 12
      Top = 144
      Width = 441
      Height = 17
      Caption = 'Split vault into few files of same size'
      TabOrder = 2
      OnClick = chbPartedClick
    end
    object edtPartSize: TEdit
      Left = 152
      Top = 166
      Width = 65
      Height = 21
      Enabled = False
      TabOrder = 3
      Text = '1024'
      OnExit = edtPartSizeExit
    end
    object spnPartSize: TUpDown
      Left = 217
      Top = 166
      Width = 16
      Height = 21
      Associate = edtPartSize
      Enabled = False
      Min = 64
      Max = 32767
      Position = 1024
      TabOrder = 4
      Thousands = False
    end
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 258
    Width = 465
    Height = 48
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object bvlButtonsTop: TBevel
      Left = 0
      Top = 0
      Width = 465
      Height = 4
      Align = alTop
      Shape = bsTopLine
    end
    object btnBack: TButton
      Left = 224
      Top = 15
      Width = 75
      Height = 23
      Caption = '< &Back'
      Enabled = False
      TabOrder = 0
      OnClick = btnBackClick
    end
    object btnNext: TButton
      Left = 299
      Top = 15
      Width = 75
      Height = 23
      Caption = '&Next >'
      Default = True
      TabOrder = 2
      OnClick = btnNextClick
    end
    object btnCancel: TButton
      Left = 383
      Top = 15
      Width = 75
      Height = 23
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 3
    end
    object btnFinish: TButton
      Left = 299
      Top = 15
      Width = 75
      Height = 23
      Caption = '&Finish'
      Default = True
      Enabled = False
      ModalResult = 1
      TabOrder = 1
      Visible = False
    end
  end
  object dlgSave: TSaveDialog
    DefaultExt = '.svlt'
    Filter = 'Vault (*.svlt)|*.svlt|Any File (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 420
    Top = 124
  end
end
