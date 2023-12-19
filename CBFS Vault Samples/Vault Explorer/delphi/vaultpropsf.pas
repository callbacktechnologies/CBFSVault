unit vaultpropsf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls,
  vaultencryptionf, strres,
  cbvcore, cbvcbvault, cbvconstants;

type
  TFormVaultprops = class(TForm)
    pnlButtons: TPanel;
    btnOK: TButton;
    btnCancel: TButton;
    PageControl1: TPageControl;
    tabGeneral: TTabSheet;
    imgIcon: TImage;
    bvl2: TBevel;
    lblType: TLabel;
    bvl3: TBevel;
    Panel1: TPanel;
    Panel2: TPanel;
    lblTypeValue: TLabel;
    lblLogo: TLabel;
    lblUsed: TLabel;
    lblUsedBytes: TLabel;
    lblUsedValue: TLabel;
    lblFreeSpace: TLabel;
    lblFreeBytes: TLabel;
    lblFreeValue: TLabel;
    lblCapacity: TLabel;
    lblCapaBytes: TLabel;
    lblCapaValue: TLabel;
    lblVaultName: TLabel;
    lblPageSize: TLabel;
    lblPageSizeValue: TLabel;
    btnAdvanced: TButton;
    procedure btnAdvancedClick(Sender: TObject);
  private
    FEncryption: Integer;
    FPassword: WideString;
    FFileName: WideString;
  public
    property FileName: WideString read FFileName write FFileName;
    property Encryption: Integer read FEncryption write FEncryption;
    property Password: WideString read FPassword write FPassword;
  end;

var
  FormVaultprops: TFormVaultprops;

implementation

{$R *.DFM}

procedure TFormVaultprops.btnAdvancedClick(Sender: TObject);
begin
  with TFormVaultencryption.Create(Self) do
    try
      lbText.Caption := Format(VAULT_PROPS_SelectMessage, [string(FileName)]);
      EncryptionChecked := (Encryption <> VAULT_EM_NONE);
      cbEncryption.Checked := (Encryption <> VAULT_EM_NONE);
      edPassword.Text := Password;
      edConfirmation.Text := Password;
      if ShowModal = mrOK then
      begin
        if cbEncryption.Checked then
        begin
          Encryption := VAULT_EM_DEFAULT;
          Password := edPassword.Text;
        end
        else
        begin
          Encryption := VAULT_EM_NONE;
          Password := '';
        end;
      end;
    finally
      Free;
    end;
end;

end.

