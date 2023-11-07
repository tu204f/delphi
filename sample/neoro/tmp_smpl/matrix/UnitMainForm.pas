unit UnitMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Grids, Vcl.ComCtrls,
  Lb.NeuronNet.SysUtils;

type
  TMainForm = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    StrGrid: TStringGrid;
    UpDownN: TUpDown;
    UpDownM: TUpDown;
    StrGrid2: TStringGrid;
    UpDownM2: TUpDown;
    UpDownN2: TUpDown;
    Label3: TLabel;
    Label4: TLabel;
    Edit3: TEdit;
    Edit4: TEdit;
    Button2: TButton;
    Button3: TButton;
    StrGridResult: TStringGrid;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    MatrixLeft, MatrixRight, MatrixResult: TMatrix;
    procedure SetStrGridMatrix(AStrGrid: TStringGrid; AMatrix: TMatrix);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.SetStrGridMatrix(AStrGrid: TStringGrid; AMatrix: TMatrix);
var
  i, iCount: Integer;
  j, jCount: Integer;
begin
  AStrGrid.RowCount := AMatrix.CountM;
  AStrGrid.ColCount := AMatrix.CountN;

  iCount := AMatrix.CountN;
  jCount := AMatrix.CountM;

  for i := 0 to iCount - 1 do
    for j := 0 to jCount - 1 do
      AStrGrid.Cells[i,j] := FloatToStr(AMatrix.Values[i,j]);

end;


procedure TMainForm.Button1Click(Sender: TObject);
begin
  MatrixLeft.MatrixSize(
    UpDownN.Position,
    UpDownM.Position
  );
  MatrixLeft.RandomValue;
  SetStrGridMatrix(StrGrid,MatrixLeft);
end;


procedure TMainForm.Button2Click(Sender: TObject);
begin
  MatrixRight.MatrixSize(
    UpDownN2.Position,
    UpDownM2.Position
  );
  MatrixRight.RandomValue;
  SetStrGridMatrix(StrGrid2,MatrixRight);
end;

procedure TMainForm.Button3Click(Sender: TObject);
begin
  // Умноженеи одной матрицы на другую
end;

end.
