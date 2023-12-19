unit aboutf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, cbvcbvault;

type
  TFormAbout = class(TForm)
    lblMain: TLabel;
    imgIcon: TImage;
    btnOK: TButton;
    lblCopyright: TLabel;
    lblVersion: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormAbout: TFormAbout;

implementation

{$R *.DFM}

procedure TFormAbout.FormCreate(Sender: TObject);
var
  Version: String;
  Vault: TcbvCBVault;
begin
  Vault := TcbvCBVault.Create(Self);
  Version := Vault.Config('BuildInfo');
  lblVersion.Caption := Format(lblVersion.Caption, [Version]);
  Delete(Version, 3, 1000);
  lblCopyright.Caption := Format(lblCopyright.Caption, [Version]);
end;

end.
