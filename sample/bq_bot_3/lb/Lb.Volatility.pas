unit Lb.Volatility;

interface


uses
  System.Classes,
  System.SysUtils,
  System.math,
  System.Threading,
  System.DateUtils,
  System.SyncObjs,
  System.Generics.Collections,
  Lb.SysUtils;

type
  TVoltParam = record
    High: Double;
    Low: Double;
    Delta: Double;
  end;
  TVoltParamList = TList<TVoltParam>;

  TVolatility = class(TObject)
  private
    FVoltParams: TVoltParamList;
    FCandels: TCandelList;
    procedure SetCandels(const Value: TCandelList);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Candels: TCandelList read FCandels write SetCandels;
  end;

implementation

{ TVolatility }

constructor TVolatility.Create;
begin
  FCandels := TCandelList.Create;
  FVoltParams := TVoltParamList.Create;
end;

destructor TVolatility.Destroy;
begin
  FreeAndNil(FVoltParams);
  FreeAndNil(FCandels);
  inherited;
end;

procedure TVolatility.SetCandels(const Value: TCandelList);
var
  xCandel: TCandel;
  xVoltParam: TVoltParam;
begin
  FCandels.CopyCandels(Value);
  for xCandel in FCandels do
  begin
    xVoltParam.High  := (xCandel.High - xCandel.Open);
    xVoltParam.Low   := (xCandel.Open - xCandel.Low);
    xVoltParam.Delta := (xCandel.High - xCandel.Low);
    FVoltParams.Add(xVoltParam);
  end;
end;

end.
