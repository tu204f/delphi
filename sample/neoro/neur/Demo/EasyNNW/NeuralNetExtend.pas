{***************************************************}
{           (c) BaseGroup Lab. 2000                 }
{  ��������� ����, ��������� �� �������� ���������  }
{  ��������������� ������.                          }
{  �������� ������ Neural Network Wizard.           }
{***************************************************}

unit NeuralNetExtend;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, NeuralBaseComp, ExtCtrls, StdCtrls, Spin, Grids, NeuralBaseTypes,
  IniFiles;

const
  FormCaption = '�������� NNW';

type
  TfrmNeuralNetExtend = class(TForm)
    NeuralNetExtended: TNeuralNetExtended;
    PageControl: TPageControl;
    pnlNavigation: TPanel;
    Tab1: TTabSheet;
    btnBack: TButton;
    rgrFileType: TRadioGroup;
    btnNext: TButton;
    btnCancel: TButton;
    Tab2: TTabSheet;
    lblFileName: TLabel;
    btnOpenFile: TButton;
    edtFileName: TEdit;
    OpenDialog: TOpenDialog;
    Tab3: TTabSheet;
    ltbFieldName: TListBox;
    Label2: TLabel;
    rdgFieldType: TRadioGroup;
    rdgNormType: TRadioGroup;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    edtMin: TEdit;
    Label4: TLabel;
    edtMax: TEdit;
    Label5: TLabel;
    edtA: TEdit;
    Tab4: TTabSheet;
    speLayers: TSpinEdit;
    Label6: TLabel;
    stgNeuronsInLayer: TStringGrid;
    Label7: TLabel;
    Tab5: TTabSheet;
    Label8: TLabel;
    tbrAlpha: TTrackBar;
    sttAlpha: TStaticText;
    Label9: TLabel;
    edtMomentum: TEdit;
    Label10: TLabel;
    edtTeachRate: TEdit;
    Tab6: TTabSheet;
    Label11: TLabel;
    Label12: TLabel;
    btnContinueTeach: TButton;
    sttMaxTeachError: TStaticText;
    sttEpochCount: TStaticText;
    GroupBox2: TGroupBox;
    Label13: TLabel;
    edtIdentError: TEdit;
    speEpochCount: TSpinEdit;
    cbxEpoch: TCheckBox;
    cbxMaxTeachError: TCheckBox;
    edtMaxTeachErrorValue: TEdit;
    cbxMidTeachError: TCheckBox;
    edtMidTeachErrorValue: TEdit;
    cbxTeachIdent: TCheckBox;
    speTeachIdentValue: TSpinEdit;
    btnBeginTeach: TButton;
    cbxMaxTestError: TCheckBox;
    cbxMidTestError: TCheckBox;
    cbxTestIdent: TCheckBox;
    edtMaxTestErrorValue: TEdit;
    edtMidTestErrorValue: TEdit;
    speTestIdentValue: TSpinEdit;
    Tab7: TTabSheet;
    Label14: TLabel;
    stgInput: TStringGrid;
    Label15: TLabel;
    stgOutput: TStringGrid;
    btnCompute: TButton;
    Memo1: TMemo;
    Label16: TLabel;
    Label17: TLabel;
    Memo2: TMemo;
    Bevel1: TBevel;
    Memo3: TMemo;
    Label1: TLabel;
    sttMaxTestError: TStaticText;
    Label18: TLabel;
    sttMidTeachError: TStaticText;
    Label19: TLabel;
    sttMidTestError: TStaticText;
    SaveDialog: TSaveDialog;
    edtUseForTeach: TEdit;
    Label20: TLabel;
    btnSave: TButton;
    procedure btnCancelClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnBackClick(Sender: TObject);
    procedure btnOpenFileClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure ltbFieldNameClick(Sender: TObject);
    procedure rdgFieldTypeClick(Sender: TObject);
    procedure rdgNormTypeClick(Sender: TObject);
    procedure edtAChange(Sender: TObject);
    procedure tbrAlphaChange(Sender: TObject);
    procedure edtMomentumChange(Sender: TObject);
    procedure edtTeachRateChange(Sender: TObject);
    procedure NeuralNetExtendedEpochPassed(Sender: TObject);
    procedure btnContinueTeachClick(Sender: TObject);
    procedure edtIdentErrorChange(Sender: TObject);
    procedure cbxEpochClick(Sender: TObject);
    procedure speEpochCountChange(Sender: TObject);
    procedure cbxMaxTeachErrorClick(Sender: TObject);
    procedure edtMaxTeachErrorValueChange(Sender: TObject);
    procedure cbxMidTeachErrorClick(Sender: TObject);
    procedure edtMidTeachErrorValueChange(Sender: TObject);
    procedure cbxTeachIdentClick(Sender: TObject);
    procedure speTeachIdentValueChange(Sender: TObject);
    procedure cbxMaxTestErrorClick(Sender: TObject);
    procedure edtMaxTestErrorValueChange(Sender: TObject);
    procedure cbxMidTestErrorClick(Sender: TObject);
    procedure edtMidTestErrorValueChange(Sender: TObject);
    procedure cbxTestIdentClick(Sender: TObject);
    procedure speTestIdentValueChange(Sender: TObject);
    procedure NeuralNetExtendedAfterTeach(Sender: TObject);
    procedure btnBeginTeachClick(Sender: TObject);
    procedure btnComputeClick(Sender: TObject);
    procedure speLayersChange(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure edtUseForTeachChange(Sender: TObject);
  private
    { Private declarations }
    Teach: boolean;
    NotSaved: boolean;
    procedure ChangePage;
    procedure OpenFile;
    procedure LoadFile;
    procedure Tune;
    procedure CreateNet;
    procedure RunTeach;
    procedure TestNet;
  public
    { Public declarations }
  end;

var
  frmNeuralNetExtend: TfrmNeuralNetExtend;
implementation

{$R *.DFM}

// ������ ���������
procedure TfrmNeuralNetExtend.FormActivate(Sender: TObject);
begin
  Caption := FormCaption;
  PageControl.ActivePage := PageControl.Pages[0];
  Teach := false;
  NotSaved := false;
end;

// ����� �� ���������
procedure TfrmNeuralNetExtend.btnCancelClick(Sender: TObject);
begin
  if NotSaved then
    if MessageDlg('������� ������������� ������. ���������?',mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      btnSave.Click;
  Close;
end;

// ������� ������ "������"
procedure TfrmNeuralNetExtend.btnNextClick(Sender: TObject);
begin
  with PageControl do
  begin
    ActivePage := FindNextPage(ActivePage, true, false);
    ChangePage;
    if ActivePage.PageIndex = PageCount - 1 then
      btnNext.Enabled := false
    else
      btnNext.Enabled := true;
    btnBack.Enabled := true;
  end;
end;

// ������� ������ "�����"
procedure TfrmNeuralNetExtend.btnBackClick(Sender: TObject);
begin
  with PageControl do
  begin
    ActivePage := FindNextPage(ActivePage, false, false);
    if ActivePage.PageIndex = 0 then
      btnBack.Enabled := false
    else
      btnBack.Enabled := true;
    btnNext.Enabled := true;
  end;
end;

// �������� �� ����� ��������
procedure TfrmNeuralNetExtend.ChangePage;
begin
  case PageControl.ActivePage.PageIndex of
    1: OpenFile;
    2: LoadFile;
    3: Tune;
    4: CreateNet;
    6: TestNet;
  end;
end;

// ������� ���� - �������� ������
procedure TfrmNeuralNetExtend.OpenFile;
begin
  if rgrFileType.ItemIndex = 0 then
  begin
    OpenDialog.Filter := 'NNW files (*.nnw)|*.nnw';
    lblFileName.Caption := '�������� nnw-����';
  end
  else
  begin
    OpenDialog.Filter := 'Text files (*.txt)|*.txt';
    lblFileName.Caption := '�������� txt-����';
  end;
end;

// ����� ����� � ���������� ����
procedure TfrmNeuralNetExtend.btnOpenFileClick(Sender: TObject);
begin
  OpenDialog.Execute;
  Caption := FormCaption + ' - ' + ExtractFileName(OpenDialog.FileName);
  edtFileName.Text := OpenDialog.FileName;
end;

// ��������� � ��������� ��������� ����
procedure TfrmNeuralNetExtend.LoadFile;
var
  i: integer;
begin
  try
    if rgrFileType.ItemIndex = 0 then
      NeuralNetExtended.FileName := edtFileName.Text   // nnw-����
    else
    begin
      NeuralNetExtended.SourceFileName := edtFileName.Text; // ��������� ����
      NeuralNetExtended.LoadDataFrom; // ��������� ������ �� ���������� �����
      // ������������ ��������� ���� �� ���������
      NeuralNetExtended.AddLayer(2);
      NeuralNetExtended.AddLayer(3);
      NeuralNetExtended.AddLayer(1);
    end;
  except
    raise Exception.Create('������ ��� �������� �����');
  end;
  NeuralNetExtended.Init;  // ������������� ����
  // ������������ ������ ����� ��� StringList-�
  ltbFieldName.Clear;
  for i := 0 to NeuralNetExtended.AvailableFieldsCount - 1 do
    ltbFieldName.Items.Add(NeuralNetExtended.Fields[i].Name);
  ltbFieldName.ItemIndex := 0;
  ltbFieldNameClick(Self);
end;

// ��������� ���������� ����
procedure TfrmNeuralNetExtend.Tune;
var
  i: integer;
begin
  speLayers.Value := NeuralNetExtended.LayerCount - 2;
  stgNeuronsInLayer.RowCount := speLayers.Value + 1;
  stgNeuronsInLayer.Cells[0, 0] := '� ����';
  stgNeuronsInLayer.Cells[1, 0] := '��������';
  for i := 0 to speLayers.Value - 1 do
  begin
    stgNeuronsInLayer.Cells[0, i + 1] := IntToStr(i);
    stgNeuronsInLayer.Cells[1, i + 1] := IntToStr(NeuralNetExtended.Layers[i + 1].NeuronCount);
  end;

  tbrAlpha.Position := trunc(NeuralNetExtended.Alpha * 100);
  edtMomentum.Text := FloatToStr(NeuralNetExtended.Momentum);
  edtTeachRate.Text := FloatToStr(NeuralNetExtended.TeachRate);
  edtIdentError.Text := FloatToStr(NeuralNetExtended.IdentError);
  edtUseForTeach.Text := FloatToStr(NeuralNetExtended.UseForTeach);

  cbxEpoch.Checked := NeuralNetExtended.Epoch;
  speEpochCount.Text := IntToStr(NeuralNetExtended.EpochCount);

  cbxMaxTeachError.Checked := NeuralNetExtended.MaxTeachError;
  edtMaxTeachErrorValue.Text := FloatToStr(NeuralNetExtended.MaxTeachErrorValue);

  cbxMidTeachError.Checked := NeuralNetExtended.MidTeachError;
  edtMidTeachErrorValue.Text := FloatToStr(NeuralNetExtended.MidTeachErrorValue);

  cbxTeachIdent.Checked := NeuralNetExtended.TeachIdent;
  speTeachIdentValue.Value := NeuralNetExtended.TeachIdentCount;

  cbxMaxTestError.Checked := NeuralNetExtended.MaxTestError;
  edtMaxTestErrorValue.Text := FloatToStr(NeuralNetExtended.MaxTestErrorValue);

  cbxMidTestError.Checked := NeuralNetExtended.MidTestError;
  edtMidTestErrorValue.Text := FloatToStr(NeuralNetExtended.MidTestErrorValue);

  cbxTestIdent.Checked := NeuralNetExtended.TestIdent;
  speTestIdentValue.Value := NeuralNetExtended.TeachIdentCount;
end;

// ����������� ���������� � ��������� ���� (��� ����, ������������ � ������)
procedure TfrmNeuralNetExtend.ltbFieldNameClick(Sender: TObject);
begin
  // ��� ���� - �������, ��������, �� ������������
  rdgFieldType.ItemIndex := NeuralNetExtended.Fields[ltbFieldName.ItemIndex].Kind;
  // ��� ������������
  rdgNormType.ItemIndex := NeuralNetExtended.Fields[ltbFieldName.ItemIndex].NormType;
  // �������� ������������
  edtA.Text := FloatToStr(NeuralNetExtended.Fields[ltbFieldName.ItemIndex].Alpha);
  // ������� � �������� (��� �������� ������������)
  edtMin.Text := FloatToStr(NeuralNetExtended.Fields[ltbFieldName.ItemIndex].ValueMin);
  edtMax.Text := FloatToStr(NeuralNetExtended.Fields[ltbFieldName.ItemIndex].ValueMax);
end;

// �������� ��� ����
procedure TfrmNeuralNetExtend.rdgFieldTypeClick(Sender: TObject);
begin
  NeuralNetExtended.Fields[ltbFieldName.ItemIndex].Kind := rdgFieldType.ItemIndex
end;

// �������� ��� ������������
procedure TfrmNeuralNetExtend.rdgNormTypeClick(Sender: TObject);
begin
  NeuralNetExtended.Fields[ltbFieldName.ItemIndex].NormType := rdgNormType.ItemIndex
end;

// �������� �������� ������������
procedure TfrmNeuralNetExtend.edtAChange(Sender: TObject);
begin
 NeuralNetExtended.Fields[ltbFieldName.ItemIndex].Alpha := StrToFloat(edtA.Text);
end;

// �������� �������� Alpha ����
procedure TfrmNeuralNetExtend.tbrAlphaChange(Sender: TObject);
begin
  sttAlpha.Caption := FloatToStr(tbrAlpha.Position / 100);
end;

// ��������� ���������� ������� �����
procedure TfrmNeuralNetExtend.speLayersChange(Sender: TObject);
begin
  stgNeuronsInLayer.RowCount := speLayers.Value + 1;
end;

// �������� ���� ��������� ���������
procedure TfrmNeuralNetExtend.CreateNet;
var
  i: integer;
  xInput, xOutput: integer;
begin
  // ���������� ������ ���������� �������� � ������� �����,
  // ���������� �������� �� ������� � �������� ���� ������� ��
  // ����� �����
  with NeuralNetExtended do
  begin
    xInput := InputFieldCount;
    xOutput := OutputFieldCount;
    ResetLayers;
    AddLayer(xInput);
    for i := 0 to speLayers.Value - 1 do
      AddLayer(StrToInt(stgNeuronsInLayer.Cells[1, i + 1]));
    AddLayer(xOutput);
  end;
end;

// �������� ������ ����
procedure TfrmNeuralNetExtend.edtMomentumChange(Sender: TObject);
begin
  NeuralNetExtended.Momentum := StrToFloat(edtMomentum.Text);
end;

// �������� �������� �������� ����
procedure TfrmNeuralNetExtend.edtTeachRateChange(Sender: TObject);
begin
  NeuralNetExtended.TeachRate := StrToFloat(edtTeachRate.Text);
end;

// �������� �� ����������� ����� ����� ��������
procedure TfrmNeuralNetExtend.NeuralNetExtendedEpochPassed(Sender: TObject);
begin
  sttMaxTestError.Caption := '';
  sttMidTestError.Caption := '';
  with NeuralNetExtended do
  begin
    sttMaxTeachError.Caption := FloatToStr(MaxTeachResidual); // �������� ����. ������ �� ��������� ���������
    sttMidTeachError.Caption := FloatToStr(MidTeachResidual); // �������� ����. ������ �� ��������� ���������
    if NeuralNetExtended.UseForTeach <> 100 then
    begin
      sttMaxTestError.Caption := FloatToStr(MaxTestResidual); // �������� ����. ������ �� �������� ���������
      sttMidTestError.Caption := FloatToStr(MidTestResidual); // �������� ����. ������ �� �������� ���������
    end;
    sttEpochCount.Caption := FloatToStr(EpochCurrent); // �������� ����� ������� �����
  end;
  Application.ProcessMessages; // ���� ����������� Windows ������������ �����
end;

// ������� ������ "���������� ��������"
procedure TfrmNeuralNetExtend.btnContinueTeachClick(Sender: TObject);
begin
  NeuralNetExtended.ContinueTeach := true; // �������� ���� - "���������� ��������"
  RunTeach;  // ������
end;

// ������� ������ "�������"
procedure TfrmNeuralNetExtend.btnBeginTeachClick(Sender: TObject);
begin
  NeuralNetExtended.ContinueTeach := false; // �������� ���� - "������ �������� �����"
  RunTeach;
end;

procedure TfrmNeuralNetExtend.edtIdentErrorChange(Sender: TObject);
begin
  NeuralNetExtended.IdentError := StrToFloat(edtIdentError.Text);
end;

procedure TfrmNeuralNetExtend.edtUseForTeachChange(Sender: TObject);
begin
  NeuralNetExtended.UseForTeach := StrToInt(edtUseForTeach.Text);
end;

procedure TfrmNeuralNetExtend.cbxEpochClick(Sender: TObject);
begin
  NeuralNetExtended.Epoch := cbxEpoch.Checked;
end;

procedure TfrmNeuralNetExtend.speEpochCountChange(Sender: TObject);
begin
  NeuralNetExtended.EpochCount := StrToInt(speEpochCount.Text);
end;

procedure TfrmNeuralNetExtend.cbxMaxTeachErrorClick(Sender: TObject);
begin
  NeuralNetExtended.MaxTeachError := cbxMaxTeachError.Checked;
end;

procedure TfrmNeuralNetExtend.edtMaxTeachErrorValueChange(Sender: TObject);
begin
  NeuralNetExtended.MaxTeachErrorValue := StrToFloat(edtMaxTeachErrorValue.Text);
end;

procedure TfrmNeuralNetExtend.cbxMidTeachErrorClick(Sender: TObject);
begin
  NeuralNetExtended.MidTeachError := cbxMidTeachError.Checked;
end;

procedure TfrmNeuralNetExtend.edtMidTeachErrorValueChange(Sender: TObject);
begin
  NeuralNetExtended.MidTeachErrorValue := StrToFloat(edtMidTeachErrorValue.Text);
end;

procedure TfrmNeuralNetExtend.cbxTeachIdentClick(Sender: TObject);
begin
  NeuralNetExtended.TeachIdent := cbxTeachIdent.Checked;
end;

procedure TfrmNeuralNetExtend.speTeachIdentValueChange(Sender: TObject);
begin
  NeuralNetExtended.TeachIdentCount := speTeachIdentValue.Value;
end;

procedure TfrmNeuralNetExtend.cbxMaxTestErrorClick(Sender: TObject);
begin
  NeuralNetExtended.MaxTestError := cbxMaxTestError.Checked;
end;

procedure TfrmNeuralNetExtend.edtMaxTestErrorValueChange(Sender: TObject);
begin
  NeuralNetExtended.MaxTestErrorValue := StrToFloat(edtMaxTestErrorValue.Text);
end;

procedure TfrmNeuralNetExtend.cbxMidTestErrorClick(Sender: TObject);
begin
  NeuralNetExtended.MidTestError := cbxMidTestError.Checked;
end;

procedure TfrmNeuralNetExtend.edtMidTestErrorValueChange(Sender: TObject);
begin
  NeuralNetExtended.MidTestErrorValue := StrToFloat(edtMidTestErrorValue.Text);
end;

procedure TfrmNeuralNetExtend.cbxTestIdentClick(Sender: TObject);
begin
  NeuralNetExtended.TestIdent := cbxTestIdent.Checked;
end;

procedure TfrmNeuralNetExtend.speTestIdentValueChange(Sender: TObject);
begin
  NeuralNetExtended.TeachIdentCount := speTestIdentValue.Value;
end;

// ������������ �� ��������� ��������
procedure TfrmNeuralNetExtend.NeuralNetExtendedAfterTeach(Sender: TObject);
begin
  btnBack.Enabled := true;
  btnNext.Enabled := true;
  btnCancel.Enabled := true;
  btnContinueTeach.Caption := '�������';
  NeuralNetExtended.StopTeach := true;
end;

procedure TfrmNeuralNetExtend.RunTeach;
begin
  Teach := not Teach;  // ������������� ��������� "������/�� ������"
  if Teach then
  begin
    btnBeginTeach.Enabled := false;
    btnBack.Enabled := false;
    btnNext.Enabled := false;
    btnCancel.Enabled := false;
    NeuralNetExtended.StopTeach := false;
    btnContinueTeach.Caption := '���������� ��������';
    NotSaved := true;
    NeuralNetExtended.Train;  // ������ ��������� �� ��������
    btnCompute.Enabled := true;
    btnSave.Enabled := true;
  end
  else
  begin
    btnBeginTeach.Enabled := true;
    btnBack.Enabled := true;
    btnNext.Enabled := true;
    btnCancel.Enabled := true;
    btnContinueTeach.Caption := '���������� ��������';
    NeuralNetExtended.StopTeach := true; // ���������� ��������
  end;
end;

// �������� �������� - ������������ ��������� ���������
procedure TfrmNeuralNetExtend.TestNet;
var
  i, j: integer;
begin
  stgInput.RowCount := NeuralNetExtended.InputFieldCount + 1;
  stgInput.Cells[0, 0] := '����';
  stgInput.Cells[1, 0] := '��������';

  // ���������� ����� ������� �����
  j := 0;
  for i := 0 to NeuralNetExtended.AvailableFieldsCount - 1 do
    if (NeuralNetExtended.Fields[i].KindName = fdInput) then // ������� ����, ��� ���� �������
    begin
      Inc(j);
      stgInput.Cells[0, j] := NeuralNetExtended.Fields[i].Name;
    end;
  stgOutput.RowCount := NeuralNetExtended.OutputFieldCount + 1;
  stgOutput.Cells[0, 0] := '����';
  stgOutput.Cells[1, 0] := '��������';

  // ���������� ����� �������� �����
  j := 0;
  for i := 0 to NeuralNetExtended.AvailableFieldsCount - 1 do
    if (NeuralNetExtended.Fields[i].KindName = fdOutput) then  // ������� ����, ��� ���� ��������
    begin
      Inc(j);
      stgOutput.Cells[0, j] := NeuralNetExtended.Fields[i].Name;
    end;
end;

// ������� ������ "���������"
procedure TfrmNeuralNetExtend.btnComputeClick(Sender: TObject);
var
  xVectorFloat: TVectorFloat;
  i: integer;
begin
  // ������� ������, ������� ����� �������� �� ����
  // ������, ������ ���������� �������� �� ������� ����
  SetLength(xVectorFloat, NeuralNetExtended.InputFieldCount);
  // ��������� �������� ��������� �������
  for i := 0 to NeuralNetExtended.InputFieldCount - 1 do
    xVectorFloat[i] := StrToFloat(stgInput.Cells[1, i + 1]);
  // ������ �� ���� ���������. ���������� ����� � �������� ���� ���������
  NeuralNetExtended.ComputeUnPrepData(xVectorFloat);
  // ���������� ���������� ����������
  for i := 0 to NeuralNetExtended.OutputFieldCount - 1 do
    stgOutput.Cells[1, i + 1] := FloatToStr(NeuralNetExtended.Output[i]);
  // ���������� ������
  SetLength(xVectorFloat, 0);
  xVectorFloat := nil;
end;

// ��������� ��������� ���������
procedure TfrmNeuralNetExtend.btnSaveClick(Sender: TObject);
begin
  SaveDialog.InitialDir := ExtractFilePath(NeuralNetExtended.FileName);
  SaveDialog.FileName := ExtractFileName(NeuralNetExtended.FileName);
  if SaveDialog.Execute then
  begin
    NeuralNetExtended.NnwFile := TIniFile.Create(SaveDialog.FileName);
    NeuralNetExtended.SavePhase1;
    NeuralNetExtended.SavePhase2;
    NeuralNetExtended.SavePhase4;
    NeuralNetExtended.SaveNetwork;
    NotSaved := false;
  end;
end;

end.
