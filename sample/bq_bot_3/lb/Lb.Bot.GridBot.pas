unit Lb.Bot.GridBot;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  Lb.SysUtils,
  Lb.Platform,
  Lb.Journal.Trading,
  Lb.CustomWorkBot;

type
  TWorkGridBot = class(TCustomWorkBot)
  public type
    TTypeLevel = (tlNull, tlUp, tlDown);

    TLevel = class(TObject)
    private
      FID: Integer;
      FTriggerPrice: Double;
      FWorkGridBot: TWorkGridBot;
      FTypeLevel: TTypeLevel;
    public
      constructor Create(const AWorkGridBot: TWorkGridBot); virtual;
      destructor Destroy; override;
      procedure SetUpPrice(ALast, ABid, AAsk: Double);
      property ID: Integer read FID write FID;
      property TriggerPrice: Double read FTriggerPrice write FTriggerPrice;
      property TypeLevel: TTypeLevel read FTypeLevel write FTypeLevel;
    end;
    TLevelList = TObjectList<TLevel>;

  private
    FMaxPrice: Double;
    FMinPrice: Double;
    FStepLevel: Double;
    FLevels: TLevelList;
    function GetCount: Integer;
  protected
    function GetCreateLevel(const AID: Integer): TLevel;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure SetStartingParameters(const AMaxPrice, AMinPrice, AStepLevel: Double);
    procedure SetUpPrice(ALast, ABid, AAsk: Double);
    property Count: Integer read GetCount;
  end;

implementation

{ TWorkGridBot.TLevel }

constructor TWorkGridBot.TLevel.Create(const AWorkGridBot: TWorkGridBot);
begin
  FTypeLevel := TTypeLevel.tlNull;
  FWorkGridBot := AWorkGridBot;
end;

destructor TWorkGridBot.TLevel.Destroy;
begin

  inherited;
end;

procedure TWorkGridBot.TLevel.SetUpPrice(ALast, ABid, AAsk: Double);
begin
  case FTypeLevel of
    tlNull: ;
    tlUp  : ;
    tlDown: ;
  end;
end;

{ TWorkGridBot }

constructor TWorkGridBot.Create;
begin
  inherited;
  FLevels := TLevelList.Create;
end;

destructor TWorkGridBot.Destroy;
begin
  if Assigned(FLevels) then
    FreeAndNil(FLevels);
  inherited;
end;

function TWorkGridBot.GetCreateLevel(const AID: Integer): TLevel;
var
  xLevel: TLevel;
begin
  xLevel := TLevel.Create(Self);
  xLevel.ID := AID;
  Result := xLevel;
  FLevels.Add(xLevel);
end;

procedure TWorkGridBot.SetStartingParameters(const AMaxPrice, AMinPrice, AStepLevel: Double);
var
  xCount: Integer;
  xLevel: TLevel;
begin
  FMaxPrice  := AMaxPrice;
  FMinPrice  := AMinPrice;
  FStepLevel := AStepLevel;

  if (FMaxPrice > 0) and (FMinPrice > 0) and (FMaxPrice > FMinPrice) and (FStepLevel > 0) then
  begin
    xCount := Round((FMaxPrice - FMinPrice)/FStepLevel);
    for var i := 0 to xCount - 1 do
    begin
      xLevel := GetCreateLevel(i);
      xLevel.TriggerPrice := FMinPrice + i * FStepLevel;
    end;
  end;
end;


procedure TWorkGridBot.SetUpPrice(ALast, ABid, AAsk: Double);
var
  xLevel: TLevel;
begin
  for xLevel in FLevels do
    xLevel.SetUpPrice(ALast, ABid, AAsk);
end;

function TWorkGridBot.GetCount: Integer;
begin
  Result := FLevels.Count;
end;



end.
