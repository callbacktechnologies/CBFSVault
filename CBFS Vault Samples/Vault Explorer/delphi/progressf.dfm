object FormProgress: TFormProgress
  Left = 366
  Top = 262
  BorderStyle = bsDialog
  Caption = 'CBFS Vault Progress'
  ClientHeight = 64
  ClientWidth = 533
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poDesktopCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblText: TLabel
    Left = 12
    Top = 12
    Width = 32
    Height = 13
    Caption = 'lblText'
  end
  object pnlList: TPanel
    Left = 0
    Top = -158
    Width = 533
    Height = 222
    Align = alBottom
    BevelOuter = bvNone
    BorderWidth = 12
    Caption = 'pnlList'
    TabOrder = 2
    Visible = False
    object lstList: TListBox
      Left = 12
      Top = 12
      Width = 509
      Height = 198
      Align = alClient
      ItemHeight = 13
      TabOrder = 0
    end
  end
  object barProgress: TProgressBar
    Left = 12
    Top = 36
    Width = 425
    Height = 16
    TabOrder = 0
  end
  object btnCancel: TButton
    Left = 452
    Top = 33
    Width = 75
    Height = 23
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = btnCancelClick
  end
end
