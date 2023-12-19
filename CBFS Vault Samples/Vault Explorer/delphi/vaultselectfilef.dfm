object FormVaultselectfile :TFormVaultselectfile
  Left = 250
  Top = 126
  BorderStyle = bsDialog
  Caption = 'Select File'
  ClientHeight = 330
  ClientWidth = 414
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  DesignSize = (
    414
    330)
  PixelsPerInch = 96
  TextHeight = 13
  object lbFileName: TLabel
    Left = 8
    Top = 304
    Width = 50
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'File Name:'
  end
  object Toolbar: TToolBar
    Left = 0
    Top = 0
    Width = 414
    Height = 29
    BorderWidth = 1
    Caption = 'Toolbar'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    object tbtnUp: TToolButton
      Left = 0
      Top = 0
      Hint = 'One level up'
      Caption = 'Up'
      ImageIndex = 6
      OnClick = tbtnUpClick
    end
    object ToolButton3: TToolButton
      Left = 23
      Top = 0
      Width = 8
      Caption = 'ToolButton3'
      ImageIndex = 2
      Style = tbsSeparator
    end
    object tbtnLargeIcons: TToolButton
      Left = 31
      Top = 0
      Hint = 'Large icons|Display items using large icons'
      Caption = 'Large Icons'
      ImageIndex = 7
      OnClick = ViewModeChange
    end
    object tbtnSmallIcons: TToolButton
      Tag = 1
      Left = 54
      Top = 0
      Hint = 'Small icons|Display items using small icons'
      Caption = 'Small Icons'
      ImageIndex = 8
      OnClick = ViewModeChange
    end
    object tbtnList: TToolButton
      Tag = 2
      Left = 77
      Top = 0
      Hint = 'List|Display items in a list'
      Caption = 'List'
      ImageIndex = 9
      OnClick = ViewModeChange
    end
    object tbtnDetails: TToolButton
      Tag = 3
      Left = 100
      Top = 0
      Hint = 'Details|Display information about each item'
      Caption = 'Details'
      ImageIndex = 10
      OnClick = ViewModeChange
    end
  end
  object lvFiles: TListView
    Left = 0
    Top = 29
    Width = 414
    Height = 262
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        AutoSize = True
        Caption = 'Name'
      end
      item
        Alignment = taRightJustify
        AutoSize = True
        Caption = 'Size'
      end
      item
        AutoSize = True
        Caption = 'Modified'
      end
      item
        AutoSize = True
        Caption = 'Attributes'
      end>
    DragMode = dmAutomatic
    HideSelection = False
    SortType = stText
    TabOrder = 1
    OnChange = lvFilesChange
    OnDblClick = lvFilesDblClick
    OnDeletion = lvFilesDeletion
  end
  object edFileName: TEdit
    Left = 72
    Top = 300
    Width = 172
    Height = 21
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 2
  end
  object btnOK: TButton
    Left = 251
    Top = 298
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object btnCancel: TButton
    Left = 331
    Top = 298
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
end
