unit Lb.BlackBox;

interface

uses
  System.Classes,
  System.SysUtils,
  System.math,
  System.Threading,
  System.DateUtils,
  System.SyncObjs,
  System.Generics.Collections,
  Lb.SysUtils,
  Lb.NeuronNet.Neuron;

type
  TBlockBox = class(TObject)
  private
    FCandels: TCandelList;
    FImages: TCandelList;
    FOnStart: TNotifyEvent;
    FOnStop: TNotifyEvent;
  protected
    procedure DoStart;
    procedure DoStop;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SetUneCandels(const ACandels: TCandelList);
    property Candels: TCandelList read FCandels;
    property Images: TCandelList read FImages;
    property OnStart: TNotifyEvent write FOnStart;
    property OnStop: TNotifyEvent write FOnStop;
  end;

  TNeuroImage = class(TObject)
  private
    FSources: TDoubleList;
    FNeuronNet: TNeuronNet;
    FBlockBox: TBlockBox;
  protected
    procedure SetCreateNeuronNet;
    procedure SetLearnNeuronNet;
  public
    constructor Create(const ABlockBox: TBlockBox); virtual;
    destructor Destroy; override;
    procedure SetSourcesFull;
    function ValueNeuronNet: Double;
    property NeuronNet: TNeuronNet read FNeuronNet;
    property BlockBox: TBlockBox read FBlockBox;
  end;

  TNeuroThread = class(TNeuroImage)
  public const
    COUNT_ITERATION = 10000;
  private
    FIsActive: Boolean;
    FTask: ITask;
    FOnStart: TNotifyEvent;
    FOnStop: TNotifyEvent;
  protected
    procedure DoStart;
    procedure DoStop;
    procedure SetThreading;
  public
    constructor Create(const ABlockBox: TBlockBox); override;
    destructor Destroy; override;
    procedure Start;
    property IsActive: Boolean read FIsActive;
    property OnStart: TNotifyEvent write FOnStart;
    property OnStop: TNotifyEvent write FOnStop;
  end;

  ///<summary>
  /// Одна коробочка
  ///<summary/>
  TBox = class(TBlockBox)
  private
    FID: Integer;
    FNeuroThread: TNeuroThread;
  public
    constructor Create; override;
    destructor Destroy; override;
    property ID: Integer read FID write FID;
    property NeuroThread: TNeuroThread read FNeuroThread;
  end;

  ///<summary>
  /// Список коробочек
  ///<summary/>
  TBoxList = TObjectList<TBox>;

implementation

procedure SetMaxAndMinPrice(const ACandels: TCandelList; var AMaxPrice, AMinPrice: Double);
var
  i, iCount: Integer;
  xCandel: TCandel;
begin
  AMaxPrice := 0;
  AMinPrice := 0;
  iCount := ACandels.Count;
  if iCount > 0 then
  begin
    xCandel := ACandels[0];
    AMaxPrice := xCandel.High;
    AMinPrice := xCandel.Low;
    for i := 1 to iCount - 1 do
    begin
      xCandel := ACandels[i];
      if xCandel.High < AMaxPrice then
        AMaxPrice := xCandel.High;
      if xCandel.Low  > AMinPrice then
        AMinPrice := xCandel.Low;
    end;
  end;
end;

procedure SetUnePrice(const ACandels, AUneCandels: TCandelList; var AMaxPrice, AMinPrice: Double);

  function _UnePrice(const APrice, AMaxPrice, AMinPrice: Double): Double;
  begin
    Result := (APrice - AMinPrice)/(AMaxPrice - AMinPrice);
  end;

var
  i, iCount: Integer;
  xCandel, xUneCandel: TCandel;
begin
  xUneCandel.Time := 0;
  xUneCandel.Vol  := 0;

  AUneCandels.Clear;
  iCount := ACandels.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xCandel := ACandels[i];
      xUneCandel.Open  := _UnePrice(xCandel.Open,AMaxPrice,AMinPrice);
      xUneCandel.High  := _UnePrice(xCandel.High,AMaxPrice,AMinPrice);
      xUneCandel.Low   := _UnePrice(xCandel.Low,AMaxPrice,AMinPrice);
      xUneCandel.Close := _UnePrice(xCandel.Close,AMaxPrice,AMinPrice);
      AUneCandels.Add(xUneCandel);
    end;
end;

{ TBlockBox }

constructor TBlockBox.Create;
begin
  FCandels := TCandelList.Create;
  FImages := TCandelList.Create;
end;

destructor TBlockBox.Destroy;
begin
  FreeAndNil(FImages);
  FreeAndNil(FCandels);
  inherited;
end;

procedure TBlockBox.SetUneCandels(const ACandels: TCandelList);
var
  xMaxPrice, xMinPrice: Double;
begin
  SetMaxAndMinPrice(ACandels, xMaxPrice, xMinPrice);
  SetUnePrice(ACandels,FCandels, xMaxPrice, xMinPrice);
  FImages.CopyCandels(FCandels);
  FImages.Delete(0);
end;

procedure TBlockBox.DoStart;
begin
  if Assigned(FOnStart) then
    FOnStart(Self);
end;

procedure TBlockBox.DoStop;
begin
  if Assigned(FOnStop) then
    FOnStop(Self);
end;

{ TNeuroImage }

constructor TNeuroImage.Create(const ABlockBox: TBlockBox);
begin
  FBlockBox := ABlockBox;
  FNeuronNet := TNeuronNet.Create;
  FSources := TDoubleList.Create;
  SetSourcesFull;
end;

destructor TNeuroImage.Destroy;
begin
  FreeAndNil(FSources);
  FreeAndNil(FNeuronNet);
  inherited;
end;

procedure TNeuroImage.SetLearnNeuronNet;
var
  xStandard, xPut: TDoubleList;
begin
  xStandard  := TDoubleList.Create;
  xPut := TDoubleList.Create;
  try
    NeuronNet.Calc(FSources,xPut);
    xStandard.GetArrayValue([1]);
    NeuronNet.CalcLearn(xStandard,xPut,0.1);
  finally
    FreeAndNil(xStandard);
    FreeAndNil(xPut);
  end;
end;

function TNeuroImage.ValueNeuronNet: Double;
var
  xPut: TDoubleList;
begin
  SetSourcesFull;
  xPut:= TDoubleList.Create;
  try
    NeuronNet.Calc(FSources,xPut);
    Result := xPut[0];
  finally
    FreeAndNil(xPut);
  end;
end;

procedure TNeuroImage.SetCreateNeuronNet;
var
  xCountLayer1, xCountLayer2: Integer;
begin
  if FBlockBox.Images.Count > 0 then
  begin
    SetSourcesFull;

    xCountLayer1 := FSources.Count;
    xCountLayer2 := FBlockBox.Images.Count;

    FNeuronNet.Clear;
    FNeuronNet.AddLayer(xCountLayer1);
    FNeuronNet.AddLayer(xCountLayer2);
    FNeuronNet.OutputLayer(1);

  end;
end;

procedure TNeuroImage.SetSourcesFull;
begin
  FSources.Clear;
  for var xC in FBlockBox.Images do
  begin
    FSources.Add(xC.Open);
    FSources.Add(xC.High);
    FSources.Add(xC.Low);
    FSources.Add(xC.Close);
    FSources.Add(xC.Vol);
  end;
end;

{ TNeuroThread }

constructor TNeuroThread.Create(const ABlockBox: TBlockBox);
begin
  inherited Create(ABlockBox);
  FIsActive := False;
end;

destructor TNeuroThread.Destroy;
begin

  inherited Destroy;
end;

procedure TNeuroThread.DoStart;
begin
  if Assigned(FOnStart) then
    FOnStart(Self);
  FBlockBox.DoStart;
end;

procedure TNeuroThread.DoStop;
begin
  FIsActive := False;
  if Assigned(FOnStop) then
    FOnStop(Self);
  FBlockBox.DoStop;
end;

procedure TNeuroThread.SetThreading;
begin
  FTask := TTask.Create(
    procedure()
    var
      i: Integer;
    begin
      i := 0;
      TThread.Synchronize(nil,DoStart);
      while i < COUNT_ITERATION do
      begin
        SetLearnNeuronNet;
        i := i + 1;
      end;
      TThread.Synchronize(nil,DoStop);
    end
  );
  FTask.Start;
end;

procedure TNeuroThread.Start;
begin
  if not FIsActive then
  begin
    SetCreateNeuronNet;
    SetThreading;
    FIsActive := True;
  end;
end;

{ TBox }

constructor TBox.Create;
begin
  inherited Create;
  FNeuroThread := TNeuroThread.Create(Self);
end;

destructor TBox.Destroy;
begin
  FreeAndNil(FNeuroThread);
  inherited Destroy;
end;

end.
