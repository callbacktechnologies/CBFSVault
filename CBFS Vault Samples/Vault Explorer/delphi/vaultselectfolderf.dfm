object FormVaultselectfolder :TFormVaultselectfolder
  Left = 296
  Top = 166
  BorderStyle = bsDialog
  Caption = 'Select Folder'
  ClientHeight = 345
  ClientWidth = 291
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    291
    345)
  PixelsPerInch = 96
  TextHeight = 13
  object lbTitle: TLabel
    Left = 8
    Top = 8
    Width = 273
    Height = 41
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'Title'
    WordWrap = True
  end
  object tvFolders: TTreeView
    Left = 8
    Top = 56
    Width = 275
    Height = 249
    Anchors = [akLeft, akTop, akRight, akBottom]
    HideSelection = False
    Indent = 19
    ReadOnly = True
    ShowRoot = False
    TabOrder = 0
    OnCollapsing = tvFoldersCollapsing
    OnDeletion = tvFoldersDeletion
    OnExpanding = tvFoldersExpanding
  end
  object btnOK: TButton
    Left = 124
    Top = 313
    Width = 77
    Height = 25
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 204
    Top = 313
    Width = 77
    Height = 25
    Anchors = [akLeft, akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
