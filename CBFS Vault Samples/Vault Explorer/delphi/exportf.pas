unit exportf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TFormExport = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    lblDiskPath: TLabel;
    edtDiskPath: TEdit;
    btnBrowse: TButton;
    chbRecursive: TCheckBox;
    edtMask: TEdit;
    lblMask: TLabel;
    procedure btnBrowseClick(Sender: TObject);
  private
  public
  end;

var
  FormExport: TFormExport;

implementation

uses
  vaultexplorerf;

{$R *.DFM}

procedure TFormExport.btnBrowseClick(Sender: TObject);
var
  Directory: string;
begin
  Directory := edtDiskPath.Text;
  if Directory = '' then
    Directory := GetCurrentDir;
  if ChooseFolder(Self, Directory, '') then
    edtDiskPath.Text := Directory;
end;

end.
