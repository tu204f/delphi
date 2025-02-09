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
  Lb.NeuronNet.Files,
  Lb.NeuronNet.Neuron;

var
  NeuronNet: TNeuronNet = nil;
  ParamNeuronNet: TParamNeuronNet = nil;

{ TMainForm }

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited;
  TableCSV := TTableCSV.Create;
  NeuronNet:= TNeuronNet.Create;
  NeuronNet.AddLayer(2);
  NeuronNet.AddLayer(15);
  NeuronNet.AddLayer(5);
  NeuronNet.OutputLayer(1);

  ParamNeuronNet := TParamNeuronNet.Create(NeuronNet);
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(ParamNeuronNet);
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

  function _ToBS(const AValue: Double): String;
  begin
    if AValue > 0.5 then
      Result := 'B'
    else
      Result := 'S';
  end;


var
  xS: String;
  xP: TTableCSV.TPosition;
  i, iCount: Integer;
  xStandard, xIn, xPut: TDoubleList;
begin
  var xFN := ExtractFilePath(ParamStr(0)) + 'param_neuron.net';
  ParamNeuronNet.Load(xFN);

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
        xIn.GetArrayValue(
          [
            xP.RSI/100,
            xP.MaRSI/100
          ]);
        NeuronNet.Calc(xIn,xPut);

        var xValue := GetRound(xPut[0]);
        case xP.Side of
          TTypeBuySell.tsBuy : xS := 'B ' + _ToBS(xValue);
          TTypeBuySell.tsSell: xS := 'S ' + _ToBS(xValue);
        end;

        ListBox1.Items.Add(xS + ' ' + xValue.ToString);
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
        xP := TableCSV.Positions[Random(iCount)];

        xStandard := TDoubleList.Create;
        xIn  := TDoubleList.Create;
        xPut := TDoubleList.Create;
        try
          xIn.GetArrayValue(
            [
              xP.RSI/100,
              xP.MaRSI/100
            ]);
          NeuronNet.Calc(xIn,xPut);

          case xP.Side of
            TTypeBuySell.tsBuy: xStandard.GetArrayValue([1]);
            TTypeBuySell.tsSell: xStandard.GetArrayValue([0]);
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
  xFN: String;
begin
  iCount := 10000;
  for i := 1 to iCount do
    _Learning;

  xFN := ExtractFilePath(ParamStr(0)) + 'param_neuron.net';
  ParamNeuronNet.Save(xFN);
end;


end.
