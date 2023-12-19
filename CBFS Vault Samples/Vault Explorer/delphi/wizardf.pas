unit wizardf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, Math,
  StrRes;

type
  TFormWizard = class(TForm)
    pnlButtons: TPanel;
    bvlButtonsTop: TBevel;
    btnBack: TButton;
    btnNext: TButton;
    btnCancel: TButton;
    pnlStep1: TPanel;
    pnlStep1Top: TPanel;
    lblStep1Caption: TLabel;
    lblStep1Description: TLabel;
    bvlStep1Top: TBevel;
    pnlStep2: TPanel;
    pnlStep2Top: TPanel;
    lblStep2Caption: TLabel;
    lblStep2Description: TLabel;
    bvlStep2Top: TBevel;
    pnlStep3: TPanel;
    pnlStep3Top: TPanel;
    lblStep3Caption: TLabel;
    lblStep3Description: TLabel;
    bvlStep3Top: TBevel;
    pnlStep4: TPanel;
    pnlStep4Top: TPanel;
    lblStep4Caption: TLabel;
    lblStep4Description: TLabel;
    bvlStep4Top: TBevel;
    pnlStep5: TPanel;
    pnlStep5Top: TPanel;
    lblStep5Caption: TLabel;
    lblStep5Description: TLabel;
    bvlStep5Top: TBevel;
    pnlStep6: TPanel;
    pnlStep6Top: TPanel;
    lblStep6Caption: TLabel;
    lblStep6Description: TLabel;
    bvlStep6Top: TBevel;

    rbtVariableSize: TRadioButton;
    rbtFixedSize: TRadioButton;
    lblStep1Prompt: TLabel;
    lblVariableSize: TLabel;
    lblFixedSize: TLabel;
    lblVaultSize: TLabel;
    edtVaultSize: TEdit;
    spnVaultSize: TUpDown;
    btnFinish: TButton;
    lblStep2Prompt: TLabel;
    chbEncryption: TCheckBox;
    edtPassword: TEdit;
    lblPassword: TLabel;
    lblConfirmation: TLabel;
    edtConfirmation: TEdit;
    lblStep4Prompt: TLabel;
    edtFileName: TEdit;
    btnBrowse: TButton;
    dlgSave: TSaveDialog;
    mmLogo: TMemo;
    lblStep5Prompt: TLabel;
    lblPageSize: TLabel;
    lblPageSizeValue: TLabel;
    cmbPageSize: TComboBox;
    chbParted: TCheckBox;
    lblPartSize: TLabel;
    edtPartSize: TEdit;
    spnPartSize: TUpDown;
    procedure btnNextClick(Sender: TObject);
    procedure btnBackClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure rbtFixedSizeClick(Sender: TObject);
    procedure pnlStep2Exit(Sender: TObject);
    procedure pnlStep2Enter(Sender: TObject);
    procedure chbEncryptionClick(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure edtFileNameChange(Sender: TObject);
    procedure pnlStep1Enter(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure pnlStep5Enter(Sender: TObject);
    procedure chbPartedClick(Sender: TObject);
    procedure cmbPageSizeChange(Sender: TObject);
    procedure pnlStep3Enter(Sender: TObject);
    procedure pnlStep3Exit(Sender: TObject);
    procedure edtPartSizeExit(Sender: TObject);
  private
    FStep: Integer;
    FCount: Integer;
    FSteps: array [1..6] of TPanel;
  public
    { Public declarations }
    FPageSize: LongWord;
    FPartSize: LongWord;
  end;

var
  FormWizard: TFormWizard;

implementation

{$R *.DFM}

procedure TFormWizard.FormCreate(Sender: TObject);
begin
  FStep := 1;
  FCount := 5;
  FSteps[1] := pnlStep1;
  FSteps[2] := pnlStep2;
  FSteps[3] := pnlStep3;
  FSteps[4] := pnlStep4;
  FSteps[5] := pnlStep5;
  FSteps[6] := pnlStep6;
  pnlStep1.Show;
end;

procedure TFormWizard.FormShow(Sender: TObject);
begin
  rbtVariableSize.SetFocus;
end;

procedure TFormWizard.btnBackClick(Sender: TObject);
begin
  if FStep = FCount then
    begin
      btnFinish.Hide;
      btnNext.Show;
    end;
    
  FSteps[FStep].Hide;
  Dec(FStep);
  FSteps[FStep].Show;
  if @FSteps[FStep].OnEnter <> nil then
    FSteps[FStep].OnEnter(nil);
  btnBack.Enabled := FStep > 1;
end;

procedure TFormWizard.btnNextClick(Sender: TObject);
begin
  if @FSteps[FStep].OnExit <> nil then
    FSteps[FStep].OnExit(nil);
  FSteps[FStep].Hide;
  Inc(FStep);
  FSteps[FStep].Show;
  if @FSteps[FStep].OnEnter <> nil then
    FSteps[FStep].OnEnter(nil);
  if FStep = FCount then
    begin
      btnFinish.Show;
      btnNext.Hide;
    end;
  btnBack.Enabled := FStep > 1;
end;

procedure TFormWizard.pnlStep1Enter(Sender: TObject);
begin
  if rbtVariableSize.Checked then
    rbtVariableSize.SetFocus
  else
    rbtFixedSize.SetFocus;
end;

procedure TFormWizard.rbtFixedSizeClick(Sender: TObject);
begin
  lblVaultSize.Enabled := rbtFixedSize.Checked;
  edtVaultSize.Enabled := rbtFixedSize.Checked;
  spnVaultSize.Enabled := rbtFixedSize.Checked;
end;

procedure TFormWizard.pnlStep2Enter(Sender: TObject);
begin
  chbEncryption.SetFocus;
end;

procedure TFormWizard.pnlStep2Exit(Sender: TObject);
begin
  if Sender = nil then
    begin
      if chbEncryption.Checked and
        (edtPassword.Text <> edtConfirmation.Text) then
        raise Exception.Create(PasswordsNotEqual);
    end;
end;

procedure TFormWizard.chbEncryptionClick(Sender: TObject);
begin
  lblPassword.Enabled := chbEncryption.Checked;
  edtPassword.Enabled := chbEncryption.Checked;
  lblConfirmation.Enabled := chbEncryption.Checked;
  edtConfirmation.Enabled := chbEncryption.Checked;
end;

procedure TFormWizard.pnlStep5Enter(Sender: TObject);
begin
  edtFileName.SetFocus;
end;

procedure TFormWizard.btnBrowseClick(Sender: TObject);
begin
  dlgSave.FileName := edtFileName.Text;
  if dlgSave.Execute then
    edtFileName.Text := dlgSave.FileName;
end;

procedure TFormWizard.edtFileNameChange(Sender: TObject);
begin
  btnFinish.Enabled := edtFileName.Text <> '';
end;

procedure TFormWizard.chbPartedClick(Sender: TObject);
begin
  lblPartSize.Enabled := chbParted.Checked;
  edtPartSize.Enabled := chbParted.Checked;
  spnPartSize.Enabled := chbParted.Checked;
end;

procedure TFormWizard.cmbPageSizeChange(Sender: TObject);
begin
  FPageSize := Round(IntPower(2, cmbPageSize.ItemIndex + 8));
  spnPartSize.Increment := FPageSize div 1024;
  if spnPartSize.Increment = 0 then
    spnPartSize.Increment := 1;
  edtPartSizeExit(Sender);
end;

procedure TFormWizard.pnlStep3Enter(Sender: TObject);
begin
  if cmbPageSize.ItemIndex = -1 then
    cmbPageSize.ItemIndex := 4;
  cmbPageSizeChange(Sender);
end;

procedure TFormWizard.pnlStep3Exit(Sender: TObject);
begin
  if chbParted.Checked then
    FPartSize := 1024 * 1024 * 1024// spnPartSize.Position * 1024
  else
    FPartSize := 0;
end;

procedure TFormWizard.edtPartSizeExit(Sender: TObject);
begin
  spnPartSize.Position := Round(spnPartSize.Position /
    spnPartSize.Increment) * spnPartSize.Increment;
end;

end.

