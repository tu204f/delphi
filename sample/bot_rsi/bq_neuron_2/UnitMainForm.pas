unit UnitMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  Lb.Table.CSV, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts,
  FMX.ListBox;

type
  TMainForm = class(TForm)
    ButtonLoad: TButton;
    ButtonLearning: TButton;
    ListBox1: TListBox;
    ButtonTest: TButton;
    procedure ButtonLoadClick(Sender: TObject);
    procedure ButtonLearningClick(Sender: TObject);
    procedure ButtonTestClick(Sender: TObject);
  private
    TableCSV: TTableCSV;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  Lb.SysUtils,
  Lb.NeuronNet.Neuron;

var
  NeuronNet: TNeuronNet = nil;

{ TMainForm }

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited;
  TableCSV := TTableCSV.Create;
  NeuronNet:= TNeuronNet.Create;
  NeuronNet.AddLayer(2);
  NeuronNet.AddLayer(15);
  NeuronNet.AddLayer(5);
  NeuronNet.OutputLayer(2);
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(NeuronNet);
  FreeAndNil(TableCSV);
  inherited;
end;

procedure TMainForm.ButtonLoadClick(Sender: TObject);
var
  xFN: String;
begin
  xFN := ExtractFilePath(ParamStr(0)) + 'position.csv';
  TableCSV.Sources.LoadFromFile(xFN)
end;

procedure TMainForm.ButtonTestClick(Sender: TObject);
var
  xS: String;
  xP: TTableCSV.TPosition;
  i, iCount: Integer;
  xStandard, xIn, xPut: TDoubleList;
begin
  ListBox1.Clear;
  iCount := TableCSV.RowCount;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xP := TableCSV.Positions[i];

      xStandard := TDoubleList.Create;
      xIn  := TDoubleList.Create;
      xPut := TDoubleList.Create;
      try
        xIn.GetArrayValue([xP.RSI,xP.MaRSI]);
        NeuronNet.Calc(xIn,xPut);
        case xP.Side of
          TTypeBuySell.tsBuy: xS := 'B ' + xPut[0].ToString + ' ' + xPut[1].ToString;
          TTypeBuySell.tsSell: xS := 'S ' + xPut[0].ToString + ' ' + xPut[1].ToString;
        end;

        ListBox1.Items.Add(xS);
      finally
        FreeAndNil(xStandard);
        FreeAndNil(xIn);
        FreeAndNil(xPut);
      end;

    end;
end;

procedure TMainForm.ButtonLearningClick(Sender: TObject);

  procedure _Learning;
  var
    xP: TTableCSV.TPosition;
    i, iCount: Integer;
    xStandard, xIn, xPut: TDoubleList;
  begin
    iCount := TableCSV.RowCount;
    if iCount > 0 then
      for i := 0 to iCount - 1 do
      begin
        xP := TableCSV.Positions[i];

        xStandard := TDoubleList.Create;
        xIn  := TDoubleList.Create;
        xPut := TDoubleList.Create;
        try
          xIn.GetArrayValue([xP.RSI,xP.MaRSI]);
          NeuronNet.Calc(xIn,xPut);

          case xP.Side of
            TTypeBuySell.tsBuy: xStandard.GetArrayValue([1,0]);
            TTypeBuySell.tsSell: xStandard.GetArrayValue([0,1]);
          end;

          NeuronNet.CalcLearn(xStandard,xPut,0.1);
        finally
          FreeAndNil(xStandard);
          FreeAndNil(xIn);
          FreeAndNil(xPut);
        end;

      end;
  end;

var
  i, iCount: Integer;
begin
  iCount := 1000;
  for i := 1 to iCount do
    _Learning;
end;


end.
