unit passwordf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TFormPassword = class(TForm)
    lbPrompt: TLabel;
    edPassword: TEdit;
    btnOK: TButton;
    btnCancel: TButton;
  private
  public
  end;

var
  FormPassword: TFormPassword;

implementation

{$R *.DFM}

end.
