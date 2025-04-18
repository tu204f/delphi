unit Lb.History.Candel;

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

function GetMathCandel(const AValues: TDoubleList; const AValueP, AValueN: Double): Double;
procedure SetHistoryCandel(const ACandels: TCandelList; const AValues: TDoubleList; var AFullCount, APositivCount: Integer);

implementation

function GetMathCandel(const AValues: TDoubleList; const AValueP, AValueN: Double): Double;
var
  xSum: Double;
begin
  xSum := 0;
  for var xV in AValues do
  begin
    if xV > 0 then
      xSum := xSum + AValueP * xV
    else
      xSum := xSum + AValueN * xV;
  end;
  Result := xSum;
end;

procedure SetHistoryCandel(const ACandels: TCandelList; const AValues: TDoubleList; var AFullCount, APositivCount: Integer);
var
  i, iCount: Integer;
  xC_R, xC_1, xC_2: TCandel;
begin
  AFullCount := 0;
  APositivCount := 0;
  iCount := ACandels.Count;
  if iCount > 0 then
    for i := 2 to iCount - 1 do
    begin
      xC_R := ACandels[i - 2];
      xC_1 := ACandels[i - 1];
      xC_2 := ACandels[i];
      if (xC_1.TypeCandel = xC_2.TypeCandel) then
      begin
        Inc(AFullCount);
        if (xC_R.TypeCandel = xC_2.TypeCandel) then
        begin
          Inc(APositivCount);
          AValues.Add(Abs(xC_R.Open - xC_R.Close));
        end
        else
          AValues.Add(-1 * Abs(xC_R.Open - xC_R.Close));
      end
      else
    end;
end;

end.
