object FormFileprops: TFormFileprops
  Left = 209
  Top = 164
  BorderStyle = bsDialog
  BorderWidth = 6
  Caption = '%s Properties'
  ClientHeight = 408
  ClientWidth = 350
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object pnlButtons: TPanel
    Left = 0
    Top = 379
    Width = 350
    Height = 29
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      350
      29)
    object btnOK: TButton
      Left = 195
      Top = 6
      Width = 75
      Height = 23
      Anchors = [akRight, akBottom]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object btnCancel: TButton
      Left = 275
      Top = 6
      Width = 75
      Height = 23
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 350
    Height = 379
    ActivePage = tabGeneral
    Align = alClient
    TabOrder = 1
    object tabGeneral: TTabSheet
      BorderWidth = 15
      Caption = 'General'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object bvl1: TBevel
        Left = 0
        Top = 0
        Width = 312
        Height = 41
        Align = alTop
        Shape = bsBottomLine
      end
      object imgIcon: TImage
        Left = 0
        Top = 0
        Width = 32
        Height = 32
      end
      object bvl2: TBevel
        Left = 0
        Top = 41
        Width = 312
        Height = 60
        Align = alTop
        Shape = bsBottomLine
      end
      object lblLocation: TLabel
        Left = 0
        Top = 52
        Width = 44
        Height = 13
        Caption = 'Location:'
      end
      object lblSize: TLabel
        Left = 0
        Top = 76
        Width = 23
        Height = 13
        Caption = 'Size:'
      end
      object bvl3: TBevel
        Left = 0
        Top = 101
        Width = 312
        Height = 84
        Align = alTop
        Shape = bsBottomLine
      end
      object lblCreated: TLabel
        Left = 0
        Top = 112
        Width = 43
        Height = 13
        Caption = 'Created:'
      end
      object lblModified: TLabel
        Left = 0
        Top = 136
        Width = 44
        Height = 13
        Caption = 'Modified:'
      end
      object lblAccessed: TLabel
        Left = 0
        Top = 160
        Width = 49
        Height = 13
        Caption = 'Accessed:'
      end
      object lblAttributes: TLabel
        Left = 0
        Top = 196
        Width = 52
        Height = 13
        Caption = 'Attributes:'
      end
      object edtFileName: TEdit
        Left = 68
        Top = 8
        Width = 243
        Height = 21
        TabOrder = 0
        Text = 'edtFileName'
      end
      object edtLocation: TEdit
        Left = 68
        Top = 52
        Width = 245
        Height = 17
        BorderStyle = bsNone
        ParentColor = True
        ReadOnly = True
        TabOrder = 1
      end
      object edtSize: TEdit
        Left = 68
        Top = 76
        Width = 245
        Height = 17
        BorderStyle = bsNone
        ParentColor = True
        ReadOnly = True
        TabOrder = 2
      end
      object edtCreated: TEdit
        Left = 68
        Top = 112
        Width = 245
        Height = 17
        BorderStyle = bsNone
        ParentColor = True
        ReadOnly = True
        TabOrder = 3
      end
      object edtModified: TEdit
        Left = 68
        Top = 136
        Width = 245
        Height = 17
        BorderStyle = bsNone
        ParentColor = True
        ReadOnly = True
        TabOrder = 4
      end
      object edtAccessed: TEdit
        Left = 68
        Top = 160
        Width = 245
        Height = 17
        BorderStyle = bsNone
        ParentColor = True
        ReadOnly = True
        TabOrder = 5
      end
      object chbDirectory: TCheckBox
        Tag = 2
        Left = 68
        Top = 196
        Width = 121
        Height = 17
        Caption = 'Directory'
        Enabled = False
        TabOrder = 6
      end
      object chbSymLink: TCheckBox
        Tag = 32
        Left = 192
        Top = 196
        Width = 121
        Height = 17
        Caption = 'SymLink'
        Enabled = False
        TabOrder = 7
      end
      object chbEncrypted: TCheckBox
        Tag = 16
        Left = 68
        Top = 220
        Width = 121
        Height = 17
        Caption = 'Encrypted'
        Enabled = False
        TabOrder = 8
      end
      object chbCompressed: TCheckBox
        Tag = 8
        Left = 192
        Top = 220
        Width = 121
        Height = 17
        Caption = 'Compressed'
        Enabled = False
        TabOrder = 9
      end
      object chbReadOnly: TCheckBox
        Tag = 64
        Left = 68
        Top = 244
        Width = 121
        Height = 17
        Caption = '&Read-only'
        TabOrder = 10
      end
      object chbArchive: TCheckBox
        Tag = 128
        Left = 192
        Top = 244
        Width = 121
        Height = 17
        Caption = '&Archive'
        TabOrder = 11
      end
      object chbHidden: TCheckBox
        Tag = 256
        Left = 68
        Top = 268
        Width = 121
        Height = 17
        Caption = '&Hidden'
        TabOrder = 12
      end
      object chbSystem: TCheckBox
        Tag = 512
        Left = 192
        Top = 268
        Width = 121
        Height = 17
        Caption = '&System'
        TabOrder = 13
      end
      object chbTemporary: TCheckBox
        Tag = 1024
        Left = 68
        Top = 292
        Width = 121
        Height = 17
        Caption = '&Temporary'
        TabOrder = 14
      end
      object btnAdvanced: TButton
        Left = 192
        Top = 296
        Width = 89
        Height = 25
        Caption = 'A&dvanced...'
        TabOrder = 15
        OnClick = btnAdvancedClick
      end
    end
    object tabLink: TTabSheet
      BorderWidth = 15
      Caption = 'Link'
      ImageIndex = 1
      OnShow = tabLinkShow
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Bevel1: TBevel
        Left = 0
        Top = 0
        Width = 312
        Height = 41
        Align = alTop
        Shape = bsBottomLine
      end
      object imgIcon2: TImage
        Left = 0
        Top = 0
        Width = 32
        Height = 32
      end
      object Bevel2: TBevel
        Left = 0
        Top = 41
        Width = 312
        Height = 70
        Align = alTop
        Shape = bsBottomLine
      end
      object lbTargetType: TLabel
        Left = 0
        Top = 56
        Width = 63
        Height = 13
        Caption = 'Target Type:'
      end
      object dlbTargetType: TLabel
        Left = 80
        Top = 56
        Width = 16
        Height = 13
        Caption = 'File'
      end
      object lbTarget: TLabel
        Left = 0
        Top = 80
        Width = 36
        Height = 13
        Caption = 'Target:'
      end
      object edFileName2: TEdit
        Left = 68
        Top = 10
        Width = 243
        Height = 21
        BorderStyle = bsNone
        Color = clBtnFace
        ReadOnly = True
        TabOrder = 0
        Text = 'edtFileName'
      end
      object edTarget: TEdit
        Left = 80
        Top = 78
        Width = 233
        Height = 21
        TabOrder = 1
        Text = 'Target'
        OnChange = edTargetChange
      end
      object btnChangeTarget: TButton
        Left = 192
        Top = 120
        Width = 115
        Height = 25
        Caption = 'Change Target...'
        TabOrder = 2
        OnClick = btnChangeTargetClick
      end
    end
  end
end
