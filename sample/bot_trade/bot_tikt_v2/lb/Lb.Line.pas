unit Lb.Line;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections;

type
  ///<summary>Ценовой уровень</summary>
  TLineTiket = record
    Price: Double;
    Vol: Double;
  public
    constructor Create(const APrice, AVol: Double);
  end;
  TLineTiketList = TList<TLineTiket>;

  ///<>
  TCustomLineTikets = class(TObject)
  private
    FTikets: TLineTiketList;
    function GetCount: Integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    function IndexOfLine(const APrice: Double): Integer;
    property Tikets: TLineTiketList read FTikets;
    property Count: Integer read GetCount;
  end;

implementation

{ TLineTiket }

constructor TLineTiket.Create(const APrice, AVol: Double);
begin
  Price := APrice;
  Vol := AVol;
end;

{ TCustomLineTikets }

constructor TCustomLineTikets.Create;
begin
  FTikets := TLineTiketList.Create;
end;

destructor TCustomLineTikets.Destroy;
begin
  FreeAndNil(FTikets);
  inherited;
end;

function TCustomLineTikets.GetCount: Integer;
begin
  Result := FTikets.Count;
end;

procedure TCustomLineTikets.Clear;
begin
  FTikets.Clear;
end;

function TCustomLineTikets.IndexOfLine(const APrice: Double): Integer;
var
  xTiket: TLineTiket;
  i, iCount: Integer;
begin
  Result := -1;
  iCount := FTikets.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xTiket := FTikets[i];
      if xTiket.Price = APrice then
      begin
        Result := i;
        Break;
      end;
    end;
end;

end.
