unit linkf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,
  vaultselectfilef, vaultselectfolderf,
  cbvcore, cbvcbvault, cbvconstants;

type
  TFormLink = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    edLinkName: TEdit;
    lbLinkName: TLabel;
    lbDestPath: TLabel;
    btnBrowse: TButton;
    btnDestBrowse: TButton;
    edDestPath: TEdit;
    procedure btnBrowseClick(Sender: TObject);
    procedure btnDestBrowseClick(Sender: TObject);
  private
    FVault: TcbvCBVault;
  public
    property Vault: TcbvCBVault read FVault write FVault;
  end;

var
FormLink :TFormLink;

implementation

uses
  vaultexplorerf;

{$R *.dfm}

procedure TFormLink.btnBrowseClick(Sender: TObject);
begin
  with TFormVaultselectfolder.Create(Self) do
    try
      AddVault(FVault);
      Vault := FVault;
      Folder := ExtractFilePathEx(edLinkName.Text, chr(FVault.PathSeparator));
      if ShowModal = mrOK then
      begin
        edLinkName.Text := Folder + chr(FVault.PathSeparator) + ExtractFileNameEx(edLinkName.Text, chr(FVault.PathSeparator));
      end;
    finally
      Free;
    end;
end;

procedure TFormLink.btnDestBrowseClick(Sender: TObject);
begin
  with TFormVaultselectfile.Create(Self) do
    try
      Vault := FVault;
      FileName := edDestPath.Text;
      if ShowModal = mrOK then
      begin
        edDestPath.Text := FileName;
      end;
    finally
      Free;
    end;
end;

end.

