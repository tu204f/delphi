unit UnitMainForm;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  System.Generics.Collections,
  Lb.NeuronNet.Neuron,
  Lb.NeuronNet.Files,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects;

type
  TParrent = class(TObject)
    BuySell: Char;
    Values: TDoubleList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetLoadParrent(const ASource: TStrings);
  end;
  TParrentList = TObjectList<TParrent>;

  TMainForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Timer: TTimer;
    Button3: TButton;
    Text1: TText;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    NeuronNet: TNeuronNet;
    Parrents: TParrentList;
    IndexParrent, IndexTraning: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

{ TParrent }

constructor TParrent.Create;
begin
  Values := TDoubleList.Create;
end;

destructor TParrent.Destroy;
begin
  FreeAndNil(Values);
  inherited;
end;

procedure TParrent.SetLoadParrent(const ASource: TStrings);
var
  xS: String;
  i, iCount: Integer;
begin
  Values.Clear;
  iCount := ASource.Count;
  if iCount > 0 then
  begin
    xS := ASource[0];
    BuySell := xS[1];
    for i := 1 to iCount - 1 do
    begin
      xS := ASource[i];
      Values.Add(xS.ToDouble);
    end;
  end;
end;

{ TMainForm }

procedure TMainForm.Button1Click(Sender: TObject);

  function _Count: Integer;
  var
    xStr: TStrings;
  begin
    var xFileName := ExtractFilePath(ParamStr(0)) + 'source\';
    xFileName := xFileName + 'param.mps';
    xStr := TStringList.Create;
    try
      xStr.LoadFromFile(xFileName);
      Result := xStr[0].ToInteger;
    finally
      FreeAndNil(xStr);
    end;
  end;

var
  i, iCount: Integer;
  xStr: TStrings;
  xParrent: TParrent;
begin
  iCount := _Count;
  for i := 0 to iCount - 1 do
  begin
    xStr := TStringList.Create;
    try
      var xFileName := ExtractFilePath(ParamStr(0)) + 'source\';
      xFileName := xFileName + i.ToString + '.mps';
      xStr.LoadFromFile(xFileName);

      xParrent := TParrent.Create;
      xParrent.SetLoadParrent(xStr);
      Parrents.Add(xParrent);

    finally
      FreeAndNil(xStr);
    end;
  end;
end;

procedure TMainForm.Button2Click(Sender: TObject);
begin
  IndexParrent := 0;
  IndexTraning := 0;
  Timer.Enabled := not Timer.Enabled;
end;

procedure TMainForm.Button3Click(Sender: TObject);
var
  xParamNeuronNet: TParamNeuronNet;
begin
  xParamNeuronNet := TParamNeuronNet.Create(NeuronNet);
  try
    var xFileName := ExtractFilePath(ParamStr(0));
    xFileName := xFileName + 'neron.net';
    xParamNeuronNet.Save(xFileName);
  finally
    FreeAndNil(xParamNeuronNet);
  end;
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited;
  NeuronNet := TNeuronNet.Create;

  NeuronNet.AddLayer(500);
  NeuronNet.AddLayer(50);
  NeuronNet.AddLayer(50);
  NeuronNet.OutputLayer(2);

  Parrents := TParrentList.Create;
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(Parrents);
  FreeAndNil(NeuronNet);
  inherited;
end;

procedure TMainForm.TimerTimer(Sender: TObject);
var
  xStandard, xPut: TDoubleList;
  iCount: Integer;
begin

  iCount := Parrents.Count;
  if iCount > IndexParrent then
  begin
    var xParrent := Parrents[IndexParrent];

    xStandard  := TDoubleList.Create;
    xPut := TDoubleList.Create;
    try
      NeuronNet.Calc(xParrent.Values,xPut);
      case xParrent.BuySell of
        'B': xStandard.GetArrayValue([1,0]);
        'S': xStandard.GetArrayValue([0,1]);
      end;
      NeuronNet.CalcLearn(xStandard,xPut,0.1);
    finally
      FreeAndNil(xStandard);
      FreeAndNil(xPut);
    end;

  end;

  IndexParrent := IndexParrent + 1;
  if iCount <= IndexParrent then
  begin
    IndexParrent := 0;
    IndexTraning := IndexTraning + 1;
  end;
  Text1.Text :=  IndexTraning.ToString + ' / ' + IndexParrent.ToString;
end;

end.
