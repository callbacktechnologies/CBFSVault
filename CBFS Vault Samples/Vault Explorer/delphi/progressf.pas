unit progressf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls;

type
  TFormProgress = class(TForm)
    barProgress: TProgressBar;
    btnCancel: TButton;
    lblText: TLabel;
    pnlList: TPanel;
    lstList: TListBox;
    procedure btnCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
  public
    FCancel: Boolean;
    FAutoHide: Boolean;
  end;

var
  FormProgress: TFormProgress;

implementation

uses
  vaultexplorerf;

{$R *.DFM}

procedure TFormProgress.btnCancelClick(Sender: TObject);
begin
  if not FAutoHide then
    begin
      Close;
      FormVaultExplorer.Enabled := true;
    end
  else
    FCancel := true;
end;

procedure TFormProgress.FormCreate(Sender: TObject);
begin
  FAutoHide := true;
end;

procedure TFormProgress.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  FAutoHide := true;
  FCancel := false;
  if pnlList.Visible then
  begin
    pnlList.Visible := false;
    ClientHeight := ClientHeight - pnlList.Height;
  end;
  lstList.Items.Clear;
  FormVaultExplorer.Enabled := true;
end;

end.
