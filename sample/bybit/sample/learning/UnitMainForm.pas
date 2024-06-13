unit UnitMainForm;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.IOUtils,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Layouts,
  FMX.ListBox,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Objects;

type
  TMainForm = class(TForm)
    ListBox1: TListBox;
    Button1: TButton;
    Timer1: TTimer;
    Text1: TText;
    Text2: TText;
    ButtonTest: TButton;
    ButtonDebug: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ButtonTestClick(Sender: TObject);
    procedure ButtonDebugClick(Sender: TObject);
  private
    { Private declarations }
  public
    CountEra, IndexFile: Integer;
    Files: TStringList;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  Lb.NeuronNet.Files,
  Lb.NeuronNet.Neuron,
  Lb.Source;

var
  Source: TSource = nil;
  NeuronNetUp: TNeuronNet = nil;

procedure neuroLearnRSI(const ANeuronNet: TNeuronNet; const L{, M, S}, UpR, DownR: Double);
var
  xStandard, xIn, xPut: TDoubleList;
begin
  xStandard  := TDoubleList.Create;
  xIn  := TDoubleList.Create;
  xPut := TDoubleList.Create;
  try
    xIn.GetArrayValue([L{, M, S}]);
    ANeuronNet.Calc(xIn,xPut);
    xStandard.GetArrayValue([UpR,DownR]);
    ANeuronNet.CalcLearn(xStandard,xPut,0.1);
  finally
    FreeAndNil(xStandard);
    FreeAndNil(xIn);
    FreeAndNil(xPut);
  end;
end;


procedure NeuroRSI(const ANeuronNet: TNeuronNet; const L{, M, S}: Double; var AUpR, ADownR: Double);
var
  //xV: Double;
  xIn, xPut: TDoubleList;
begin
  AUpR := 0;
  ADownR := 0;

  xIn := TDoubleList.Create;
  xPut:= TDoubleList.Create;
  try
    xIn.GetArrayValue([L{,M,S}]);
    ANeuronNet.Calc(xIn,xPut);
    AUpR := xPut[0];
    ADownR := xPut[1];
  finally
    FreeAndNil(xPut);
    FreeAndNil(xIn);
  end;
end;


procedure TMainForm.FormCreate(Sender: TObject);
begin
  Files := TStringList.Create;

  Source := TSource.Create;
  NeuronNetUp := TNeuronNet.Create;

  NeuronNetUp.AddLayer(1);
  NeuronNetUp.AddLayer(8);
  NeuronNetUp.AddLayer(8);
  NeuronNetUp.OutputLayer(2);
end;

procedure TMainForm.Timer1Timer(Sender: TObject);

  procedure _SetSaveNeuronNetUp;
  var
    xParam: TParamNeuronNet;
  begin
    xParam := TParamNeuronNet.Create(NeuronNetUp);
    try
      xParam.Save('neuron_up.txt')
    finally
      FreeAndNil(xParam);
    end;
  end;

var
  xS: String;
  xFileName: String;
begin
  xS := '';
  if CountEra > 0 then
  begin
    if (IndexFile >= 0) and (IndexFile < (Files.Count - 1)) then
    begin
      xFileName := Files[Random(Files.Count)];
      Source.SetOpen(xFileName);
      Source.SetDepth(-1);

      xS := xS + 'L:' + Source.LongRSI.ToString + ';';
      xS := xS + 'M:' + Source.MediumRSI.ToString + ';';
      xS := xS + 'S:' + Source.ShortSRI.ToString + ';';

      case Source.TypeSid of
        tsUp: begin
          xS := xS + 'UP';
          neuroLearnRSI(
            NeuronNetUp,
            Source.LongRSI,
            //Source.MediumRSI,
            //Source.ShortSRI,
            1,
            0
          );
        end;
        tsDown: begin
          xS := xS + 'DOWN';
          neuroLearnRSI(
            NeuronNetUp,
            Source.LongRSI,
            //Source.MediumRSI,
            //Source.ShortSRI,
            0,
            1
          );
        end;
      end;
      Inc(IndexFile);
    end
    else
    begin
      IndexFile := 0;
      CountEra := CountEra - 1;
    end;
  end
  else
  begin
    _SetSaveNeuronNetUp;
    Timer1.Enabled := False;
  end;
  Text1.Text := 'CountEra: ' + CountEra.ToString + '; IndexFile: ' + IndexFile.ToString;
  Text2.Text := xS;
end;

procedure TMainForm.ButtonDebugClick(Sender: TObject);
var
  xUpR, xDownR: Double;
begin
  NeuroRSI(
    NeuronNetUp,
    0.5,
    //0.4,
    //0.3,
    xUpR,
    xDownR
  );
  ListBox1.Items.Add(xUpR.ToString + '; ' + xDownR.ToString);
end;

procedure TMainForm.ButtonTestClick(Sender: TObject);

  procedure _SetLoadNeuronNetUp;
  var
    xParam: TParamNeuronNet;
  begin
    NeuronNetUp.Layers.Clear;
    xParam := TParamNeuronNet.Create(NeuronNetUp);
    try
      xParam.Load('neuron.txt')
    finally
      FreeAndNil(xParam);
    end;
  end;

  procedure _Test;
  var
    xS: String;
    xUpR, xDownR: Double;
    i, iCount: Integer;
    xTmp: String;
    xStr: TStrings;
  begin
    xStr := TStringList.Create;

    iCount := Files.Count;
    if iCount > 0 then
      for i := 0 to Files.Count - 1 do
      begin

        xS := Files[i];
        Source.SetOpen(xS);
        Source.SetDepth(-1);

        NeuroRSI(
          NeuronNetUp,
          Source.LongRSI,
          //Source.MediumRSI,
          //Source.ShortSRI,
          xUpR,
          xDownR
        );

        xTmp :=
          FloatToStr(Source.LongRSI) + ';';// +
//          FloatToStr(Source.MediumRSI) + ';' +
//          FloatToStr(Source.ShortSRI) + ';';

        case Source.TypeSid of
          tsUp: xTmp := xTmp + 'UP;';
          tsDown: xTmp := xTmp + 'DOWN;';
        end;
        xTmp := xTmp + FloatToStr(Source.DeltaValue) + ';';
        xTmp := xTmp + xUpR.ToString + ';';
        xTmp := xTmp + xDownR.ToString + ';';

        xStr.Add(xTmp);
      end;

    xStr.SaveToFile('result.csv');
  end;

begin
  _SetLoadNeuronNetUp;
  var xPath := ExtractFilePath(ParamStr(0)) + 'data\';
  SetPathFiles(xPath, Files);
  _Test;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FreeAndNil(Files);
  FreeAndNil(NeuronNetUp);
  FreeAndNil(Source);
end;

procedure TMainForm.Button1Click(Sender: TObject);
var
  xPath: String;
begin
  IndexFile := 0;
  CountEra := 10;
  Timer1.Enabled := True;
  xPath := ExtractFilePath(ParamStr(0)) + 'data\';
  SetPathFiles(xPath, Files);
end;

end.
