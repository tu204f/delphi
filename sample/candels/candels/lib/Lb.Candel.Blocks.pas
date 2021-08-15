unit Lb.Candel.Blocks;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  Lb.Candel.SysUtils,
  Lb.Candel.Source;

type
  ///<summary>Ѕлок Ц который отвечает за данные дл€ вектора</summary>
  TBlock = class(TObject)
  private
    FSources: TSourceCandel;
    FVectors: TSourceCandel;
  public
    constructor Create;
    destructor Destroy; override;
    ///<summary>‘ормирование вектора</summary>
    procedure SetFormationVector;
    ///<summary>»сточник данных дл€ блока</summary>
    property Sources: TSourceCandel read FSources;
    ///<summary>—оздаем, вектор</summary>
    property Vectors: TSourceCandel read FVectors;
  end;

implementation

{ TBlock }

constructor TBlock.Create;
begin
  FSources := TSourceCandel.Create;
  FVectors := TSourceCandel.Create;
end;

destructor TBlock.Destroy;
begin
  FreeAndNil(FVectors);
  FreeAndNil(FSources);
  inherited;
end;

procedure TBlock.SetFormationVector;

  function GetProcentVector(const AMaxValue, AMinValue, AValue: Double): Double;
  begin
    Result := 0;
    if (AMaxValue > 0) and (AMinValue > 0) and (AValue > 0) then
      Result := (AValue - AMinValue)/(AMaxValue - AMinValue);
  end;

var
  xCandel: TCandel;
  xVector: TCandel;
  xMaxPrice, xMinPrice: Double;
  xMaxVol, xMinVol: Integer;
begin
  FVectors.Candels.Clear;
  if FSources.Candels.Count > 0 then
  begin
    FSources.GetMaxAndMinPriceVol(xMaxPrice,xMinPrice,xMaxVol,xMinVol);
    for xCandel in FSources.Candels do
    begin
      xVector.Date  := xCandel.Date;
      xVector.Time  := xCandel.Time;
      xVector.Open  := GetProcentVector(xMaxPrice,xMinPrice,xCandel.Open);
      xVector.High  := GetProcentVector(xMaxPrice,xMinPrice,xCandel.High);
      xVector.Low   := GetProcentVector(xMaxPrice,xMinPrice,xCandel.Low);
      xVector.Close := GetProcentVector(xMaxPrice,xMinPrice,xCandel.Close);
      xVector.Vol   := Trunc(GetProcentVector(xMaxVol,xMinVol,xCandel.Vol) * 100);
      FVectors.Candels.Add(xVector);
    end;
  end;
end;

end.
