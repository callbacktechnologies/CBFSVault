unit openvaultf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls;

type
  TFormOpenvault = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    OpenDlg: TOpenDialog;
    edVaultFileName: TEdit;
    lbVaultFileName: TLabel;
    lbOpenMode: TLabel;
    cbReadOnly: TCheckBox;
    cbUseJournaling: TCheckBox;
    cbAutoCompact: TCheckBox;
    cbUseAccessTime: TCheckBox;
    edAutoCompact: TEdit;
    spnAutoCompact: TUpDown;
    btnBrowse: TButton;
    procedure btnBrowseClick(Sender: TObject);
    procedure cbReadOnlyClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cbAutoCompactClick(Sender: TObject);
  private
  public
  end;

var
FormOpenvault :TFormOpenvault;

implementation

{$R *.dfm}


procedure TFormOpenvault.btnBrowseClick(Sender: TObject);
begin
  OpenDlg.FileName := edVaultFileName.Text;
  OpenDlg.Options := OpenDlg.Options - [ofReadOnly];
  if OpenDlg.Execute then
  begin
    edVaultFileName.Text := OpenDlg.FileName;
    if ofReadOnly in OpenDlg.Options then
    begin
      cbReadOnly.Enabled := false;
      cbReadOnly.Checked := true;
    end
    else
    begin
      cbReadOnly.Enabled := true;
      cbReadOnly.Checked := false;
    end;
  end;
end;

procedure TFormOpenvault.cbReadOnlyClick(Sender: TObject);
begin
  cbUseJournaling.Enabled := not cbReadOnly.Checked;
  cbUseAccessTime.Enabled := not cbReadOnly.Checked;
  cbAutoCompact.Enabled := not cbReadOnly.Checked;
  edAutoCompact.Enabled := cbAutoCompact.Checked and not cbReadOnly.Checked;
  spnAutoCompact.Enabled := cbAutoCompact.Checked and not cbReadOnly.Checked;
end;

procedure TFormOpenvault.FormShow(Sender: TObject);
begin
  btnBrowseClick(Sender);
  if edVaultFileName.Text = '' then
  begin
    ModalResult := mrCancel;
    PostMessage(Handle, WM_CLOSE, 0, 0);
  end;
end;

procedure TFormOpenvault.cbAutoCompactClick(Sender: TObject);
begin
  edAutoCompact.Enabled := cbAutoCompact.Checked;
  spnAutoCompact.Enabled := cbAutoCompact.Checked;
end;

end.
