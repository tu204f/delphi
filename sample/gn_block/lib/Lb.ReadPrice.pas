(******************************************************************************)
(* Читаем историю цен                                                         *)
(******************************************************************************)
unit Lb.ReadPrice;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections;

type
  TDoubleList = TList<Double>;

  ///<summary>
  /// Свеча
  ///</summary>
  TCandel = record
    Date : TDateTime;
    Time : TDateTime;
    Open : Double;
    High : Double;
    Low  : Double;
    Close: Double;
    Vol  : Double;
    Text: String;
  public
    function ToStr: String;
  end;

  TCandelList = class(TList<TCandel>)
  public
    procedure Copy(ACandels: TCandelList);
  end;

  ///<summary>Источние данных</summary>
  TCandelsSource = class(TStringList)
  private
    function GetCandels(Index: Integer): TCandel;
  public
    property Candels[Index: Integer]: TCandel read GetCandels;
  end;

procedure SetCandels(const AIndex, ACount: Integer; ACandelsSource: TCandelsSource; ACandels: TCandelList);
function GetRSI(ACandels: TCandelList): Double;
function GetWilliamsR(ACandels: TCandelList): Double;
procedure SetFractal(ACandels: TCandelList; var AFractalUp, AFractalDown: Double);

implementation

uses
  System.DateUtils;

procedure SetCandels(const AIndex, ACount: Integer; ACandelsSource: TCandelsSource; ACandels: TCandelList);
var
  xCandel: TCandel;
  i, xInd: Integer;
begin
  ACandels.Clear;
  if ACount > 0 then
  begin
    for i := 0 to ACount - 1 do
    begin
      xInd := AIndex + i;
      if (xInd >= 0) and (ACandelsSource.Count > xInd) then
      begin
        xCandel := ACandelsSource.Candels[xInd];
        ACandels.Add(xCandel);
      end;
    end;
  end;
end;

procedure SetFractal(ACandels: TCandelList; var AFractalUp, AFractalDown: Double);

  function GetFractalUp(ACandels: TCandelList; AIndex: Integer): Boolean;
  var
    xC1, xC2, xC3, xC4, xC5: TCandel;
  begin
    xC1 := ACandels[AIndex + 2];
    xC2 := ACandels[AIndex + 1];
    xC3 := ACandels[AIndex];
    xC4 := ACandels[AIndex - 1];
    xC5 := ACandels[AIndex - 2];

    Result :=
      (xC3.High > xC2.High) and (xC2.High > xC1.High) and
      (xC3.High > xC4.High) and (xC4.High > xC5.High);
  end;

  function GetFractalDown(ACandels: TCandelList; AIndex: Integer): Boolean;
  var
    xC1, xC2, xC3, xC4, xC5: TCandel;
  begin
    xC1 := ACandels[AIndex + 2];
    xC2 := ACandels[AIndex + 1];
    xC3 := ACandels[AIndex];
    xC4 := ACandels[AIndex - 1];
    xC5 := ACandels[AIndex - 2];

    Result :=
      (xC3.Low < xC2.Low) and (xC2.Low < xC1.Low) and
      (xC3.Low < xC4.Low) and (xC4.Low < xC5.Low);
  end;

var
  //xIndex: Integer;
  i, iCount: Integer;
  xFractalUp, xFractalDown: Boolean;
begin
  AFractalUp := 0;
  AFractalDown := 0;

  xFractalUp := False;
  xFractalDown := False;

  iCount := ACandels.Count;
  if iCount > 5 then
  begin
    //xIndex := iCount - 3;
    for i := iCount - 3 downto 3 do
    begin
      if not xFractalUp then
      begin
        xFractalUp := GetFractalUp(ACandels,i);
        AFractalUp := ACandels[i].High;
      end;
      if not xFractalDown then
      begin
        xFractalDown := GetFractalDown(ACandels,i);
        AFractalDown := ACandels[i].Low;
      end;
    end;
  end;
end;

function GetRSI(ACandels: TCandelList): Double;

  function _SMA(const AValue: TDoubleList): Double;
  var
    xSum: Double;
    i, iCount: Integer;
  begin
    Result := 0;
    iCount := AValue.Count;
    if iCount > 0 then
    begin
      xSum := 0;
      for i := 0 to iCount - 1 do
        xSum := xSum + AValue[i];
      Result := xSum/iCount;
    end;
  end;

var
  xDelta: Double;
  xCandel1, xCandel2: TCandel;
  xU, xD: TDoubleList;
  xMaU, xMaD, xRS: Double;
begin
  Result := 0;
  if ACandels.Count > 0 then
  begin
    xU := TDoubleList.Create;
    xD := TDoubleList.Create;
    try
      xU.Add(0);
      xD.Add(0);
      for var i := 1 to ACandels.Count - 1 do
      begin
        xCandel1 := ACandels[i - 1];
        xCandel2 := ACandels[i];
        xDelta := xCandel2.Close - xCandel1.Close;
        if xDelta > 0 then
        begin
          xU.Add(xDelta);
          xD.Add(0);
        end
        else
        begin
          xU.Add(0);
          xD.Add(Abs(xDelta));
        end;
      end;

      xMaU := _SMA(xU);
      xMaD := _SMA(xD);
      xRS  := xMaU/xMaD;
      Result := 100 - 100/(1 + xRS);
    finally
      FreeAndNil(xD);
      FreeAndNil(xU);
    end;
  end;
end;


function GetWilliamsR(ACandels: TCandelList): Double;

  procedure _MaxMinValue(ACandels: TCandelList; var AClose, AValueMax, AValueMin: Double);
  var
    xCandel: TCandel;
    i, iCount: Integer;
  begin
    AValueMax := 0;
    AValueMin := 0;
    AClose    := 0;
    iCount := ACandels.Count;
    if iCount > 0 then
    begin
      xCandel := ACandels[0];
      AValueMax := xCandel.High;
      AValueMin := xCandel.Low;
      for i := 1 to iCount - 1 do
      begin
        xCandel := ACandels[i];
        if xCandel.High > AValueMax then
          AValueMax := xCandel.High;
        if xCandel.Low  < AValueMin then
          AValueMin := xCandel.Low;
      end;
      AClose := ACandels[iCount - 1].Close;
    end;
  end;

var
  xClose, xValueMax, xValueMin: Double;
begin
  _MaxMinValue(ACandels, xClose, xValueMax, xValueMin);
  if (xValueMax > 0) and (xValueMin > 0) and (xValueMax <> xValueMin) then
    Result := ((xValueMax - xClose)/(xValueMax - xValueMin)) * 100;

end;


(*******************************************************************************
// ѕарсим без даты и времени
<DATE>;<TIME>;<OPEN>;<HIGH>;<LOW>;<CLOSE>;<VOL>
20240301;090000;29508;29520;29488;29520;737
20240301;090500;29520;29535;29513;29535;318

<DATE>;<TIME>;<OPEN>;<HIGH>;<LOW>;<CLOSE>;<VOL>
123456
240701;100000;12082;12139;12081;12134;2213
240701;100500;12136;12137;12104;12118;1355
*******************************************************************************)

function GetCandelToStr(const S: String): TCandel;

  function _StrToDate(S: String): TDateTime;
  var
    xS: String;
  begin
    xS := Copy(S,5,2) + '.' + Copy(S,3,2) + '.20' + Copy(S,1,2);
    Result := StrToDate(xS);
  end;

  function _StrToTime(S: String): TDateTime;
  var
    xS: String;
  begin
    xS := Copy(S,1,2) + ':' + Copy(S,3,2) + ':' + Copy(S,5,2);
    Result := StrToTime(xS);
  end;

  procedure _Paser(const S: String; ASource: TStrings);
  begin
    ASource.Clear;
    ASource.Delimiter := ';';
    ASource.DelimitedText := S;
  end;

var
  xCandel: TCandel;
  xSource: TStrings;
  xOldChar: Char;
begin
  xSource := TStringList.Create;
  try
    _Paser(S,xSource);

    xOldChar := FormatSettings.DecimalSeparator;
    FormatSettings.DecimalSeparator := '.';

    xCandel.Date  := _StrToDate(xSource[0]);
    xCandel.Time  := _StrToTime(xSource[1]);
    xCandel.Open  := StrToFloatDef(xSource[2],0);
    xCandel.High  := StrToFloatDef(xSource[3],0);
    xCandel.Low   := StrToFloatDef(xSource[4],0);
    xCandel.Close := StrToFloatDef(xSource[5],0);
    xCandel.Vol   := StrToFloatDef(xSource[6],0);
    xCandel.Text  := S;

    Result := xCandel;
    FormatSettings.DecimalSeparator := xOldChar;
  finally
    FreeAndNil(xSource);
  end;
end;

{ TCandelList }

procedure TCandelList.Copy(ACandels: TCandelList);
var
  xCandel: TCandel;
begin
  Clear;
  for xCandel in ACandels do
    Self.Add(xCandel);
end;

{ TCandel }

function TCandel.ToStr: String;
var
  xS: String;
begin
  xS :=
    'Date: ' + DateToStr(Date) + '; ' +
    'Time: ' + TimeToStr(Time) + '; ' +
    'Open: ' + Open.ToString + '; ' +
    'High: ' + High.ToString + '; ' +
    'Low: ' + Low.ToString  + '; ' +
    'Close: ' + Close.ToString + '; ' +
    'Vol: ' + Vol.ToString + ';';
  Result := xS;
end;

{ TCandelsSource }

function TCandelsSource.GetCandels(Index: Integer): TCandel;
begin
  var xS := Self.Strings[Index];
  Result := GetCandelToStr(xS);
end;



end.
