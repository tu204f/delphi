(******************************************************************************)
(* Читаем заначение свячей                                                    *)
(******************************************************************************)
unit Lb.ReadPrice;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  Lb.SysUtils;

type
  TSourceCandels = class(TStringList)
  private
    function GetCandels(Index: Integer): TCandel;
  public
    property Candels[Index: Integer]: TCandel read GetCandels;
  end;

  TSourceTikets = class(TStringList)
  private
    FIndex: Integer;
    FTrade: TTiket;
    function GetTikets(Index: Integer): TTiket;
  public
    procedure First;
    procedure Next;
    function EOF: Boolean;
    property Tiket: TTiket read FTrade;
    property Tikets[Index: Integer]: TTiket read GetTikets;
  end;

  ///<summary>Чтение потниковых данных, из крипто биржи</summary>
  TBybitSourceTikets = class(TObject)
  public type
    TTypeSide = (Buy, Sell);
    TTrade = record
      ExecID: String;        // Execution ID
      Symbol: String;
      Price: Double;
      Size: Double;
      Side: TTypeSide;
      Time: Int64;
    public
      function _Time: TDateTime;
    end;
  private
    FTrade: TTrade;
    FFileName: String;
    FStream: TFileStream;
  protected
    procedure SetLine;
  public
    constructor Create(const AFileName: String); virtual;
    destructor Destroy; override;
    procedure First;
    procedure Next;
    function EOF: Boolean;
    property Trade: TTrade read FTrade;
  end;

  TBufferTikets = class(TObject)
  public type
    TDoubleList = TList<Double>;
  public const
    SIZE_BUFFER = 1000;
  private
    FValues: TDoubleList;
    function GetPriceMA: Double;
    function GetCount: Integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SetTiket(const APrice: Double);
    ///<summary>Средние значение</summary>
    property PriceMA: Double read GetPriceMA;

    procedure Delete(const AIndex: Integer);
    property Count: Integer read GetCount;

  end;


implementation

uses
  System.DateUtils;

(*******************************************************************************
// Парсим без даты и времени
<DATE>;<TIME>;<OPEN>;<HIGH>;<LOW>;<CLOSE>;<VOL>
231213;090000;25857.0000000;25897.0000000;25765.0000000;25802.0000000;2715
231213;090500;25802.0000000;25813.0000000;25791.0000000;25797.0000000;688
*******************************************************************************)

function GetCandelToStr(const S: String): TCandel;

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
    xCandel.Open  := StrToFloatDef(xSource[2],0);
    xCandel.High  := StrToFloatDef(xSource[3],0);
    xCandel.Low   := StrToFloatDef(xSource[4],0);
    xCandel.Close := StrToFloatDef(xSource[5],0);
    xCandel.Vol   := StrToFloatDef(xSource[6],0);

    Result := xCandel;

    FormatSettings.DecimalSeparator := xOldChar;
  finally
    FreeAndNil(xSource);
  end;
end;

(*******************************************************************************
// Парсим без даты и времени
<DATE>;<TIME>;<LAST>;<VOL>
231213;085933;25848.000000000;2
231213;085933;25848.000000000;1
*******************************************************************************)

function GetTikitToStr(const S: String): TTiket;

  procedure _Paser(const S: String; ASource: TStrings);
  begin
    ASource.Clear;
    ASource.Delimiter := ';';
    ASource.DelimitedText := S;
  end;

  function _TimeToStr(const S: String): Integer;
  var
    xH, xM, xS: String;
  begin
    xH := Copy(S,0,2);
    xM := Copy(S,3,2);
    xS := Copy(S,5,2);
    Result :=
      3600 * StrToIntDef(xH,0) +
      60 * StrToIntDef(xM,0) +
      StrToIntDef(xS,0);
  end;

var
  xTikit: TTiket;
  xSource: TStrings;
  xOldChar: Char;
begin
  xSource := TStringList.Create;
  try
    _Paser(S,xSource);
    xOldChar := FormatSettings.DecimalSeparator;
    FormatSettings.DecimalSeparator := '.';
    xTikit.Time := _TimeToStr(xSource[1]);
    xTikit.Last := StrToFloatDef(xSource[2],0);
    xTikit.Vol  := StrToFloatDef(xSource[3],0);
    Result := xTikit;
    FormatSettings.DecimalSeparator := xOldChar;
  finally
    FreeAndNil(xSource);
  end;
end;

{ TSourceCandels }

function TSourceCandels.GetCandels(Index: Integer): TCandel;
var
  xS: String;
begin
  xS := Self.Strings[Index];
  Result := GetCandelToStr(xS);
end;

{ TSourceTikets }

function TSourceTikets.GetTikets(Index: Integer): TTiket;
var
  xS: String;
begin
  xS := Self.Strings[Index];
  Result := GetTikitToStr(xS);
end;

procedure TSourceTikets.First;
begin
  FIndex := 1;
  FTrade := Tikets[FIndex];
end;

procedure TSourceTikets.Next;
begin
  FTrade := Tikets[FIndex];
  Inc(FIndex);
end;

function TSourceTikets.EOF: Boolean;
begin
  Result := FIndex >= Self.Count;
end;

{ TBybitSourceTikets }

constructor TBybitSourceTikets.Create(const AFileName: String);
begin
  FFileName := AFileName;
  FStream := TFileStream.Create(FFileName,fmOpenRead);
end;

destructor TBybitSourceTikets.Destroy;
begin
  FreeAndNil(FStream);
  inherited;
end;

function TBybitSourceTikets.EOF: Boolean;
begin
  Result := FStream.Position >= FStream.Size;
end;

procedure TBybitSourceTikets.First;
begin
  FStream.Position := 0;
  SetLine;
end;

procedure TBybitSourceTikets.Next;
begin
  SetLine;
end;

procedure TBybitSourceTikets.SetLine;
var
  xInd: Integer;       // Индекс поля
  xC, xOldC: AnsiChar; //
  xS: AnsiString;      // Значение
begin
  xOldC := #0;
  xInd := 0;
  xS := '';
  for var i := 0 to 100 do
  begin
    FStream.Read(xC,1);
    //#13 #10
    if (xOldC = #13) and (xC = #10) then
    begin
      if xInd = 5 then
        FTrade.Time := StrToInt64Def(Trim(String(xS)),0);
      Break;
    end;
    xOldC := xC;
    if xC = ';' then
    begin
      case xInd of
        0: FTrade.ExecID := String(xS);
        1: FTrade.Symbol := String(xS);
        2: FTrade.Price := StrToFloatDef(String(xS),0);
        3: FTrade.Size := StrToFloatDef(String(xS),0);
        4: begin
          case xS[1] of
            'B': FTrade.Side := TBybitSourceTikets.TTypeSide.Buy;
            'S': FTrade.Side := TBybitSourceTikets.TTypeSide.Sell;
          end;
        end;
      end;
      Inc(xInd);
      xS := '';
    end
    else
      xS := xS + xC;
  end;
end;

{ TBybitSourceTikets.TTrade }

function TBybitSourceTikets.TTrade._Time: TDateTime;
begin
  Result := UnixToDateTime(Trunc(Self.Time/1000));
end;

{ TBufferTikets }

constructor TBufferTikets.Create;
begin
  FValues := TDoubleList.Create;
end;

destructor TBufferTikets.Destroy;
begin
  FreeAndNil(FValues);
  inherited;
end;

procedure TBufferTikets.SetTiket(const APrice: Double);
begin
  FValues.Add(APrice);
  if FValues.Count >= SIZE_BUFFER then
    FValues.Delete(0);
end;

function TBufferTikets.GetCount: Integer;
begin
  Result := FValues.Count;
end;

procedure TBufferTikets.Delete(const AIndex: Integer);
begin
  FValues.Delete(AIndex);
end;

function TBufferTikets.GetPriceMA: Double;

  function _Round(const AValue: Double): Double;
  begin
    Result := Round(AValue * 100)/100;
  end;

var
  xSum: Double;
  i, iCount: Integer;
begin
  xSum := 0;
  Result := 0;
  iCount := FValues.Count;
  if iCount > 0 then
  begin
    for i := 0 to iCount - 1 do
      xSum := xSum + FValues[i];
    Result := _Round(xSum/iCount);
  end;
end;

end.
