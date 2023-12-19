unit vaultencryptionf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, 
  StrRes, ExtCtrls, ComCtrls;

type
  TFormVaultencryption = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    cbEncryption: TCheckBox;
    lbText: TLabel;
    edPassword: TEdit;
    lbPassword: TLabel;
    lbConfirmation: TLabel;
    edConfirmation: TEdit;
    procedure cbEncryptionClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
  public
    EncryptionChecked: Boolean;
  end;

var
FormVaultencryption :TFormVaultencryption;

implementation

uses
  vaultexplorerf;

{$R *.DFM}

procedure TFormVaultencryption.cbEncryptionClick(Sender: TObject);
begin
  if cbEncryption.Checked and
     not RegistrationKeyOK and not EncryptionChecked then
  begin
    ShowRegistrationKeyWarning;
    EncryptionChecked := true;
  end;

  lbPassword.Enabled := cbEncryption.Checked;
  edPassword.Enabled := cbEncryption.Checked;
  lbConfirmation.Enabled := cbEncryption.Checked;
  edConfirmation.Enabled := cbEncryption.Checked;
end;

procedure TFormVaultencryption.btnOKClick(Sender: TObject);
begin
  if edPassword.Text <> edConfirmation.Text then
  begin
    MessageDlg(PasswordsNotEqual, mtError, [mbOK], 0);
    Exit;
  end;

  ModalResult := mrOK;
end;

procedure TFormVaultencryption.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

end.
