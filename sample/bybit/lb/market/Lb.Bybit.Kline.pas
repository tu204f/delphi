(******************************************************************************)
(* Позови Клайна                                                              *)
(* Запрос исторических линий (также известных как candles/candlesticks).      *)
(* Графики возвращаются группами на основе запрошенного интервала.            *)
(******************************************************************************)
unit Lb.Bybit.Kline;

interface

uses
  System.Classes,
  System.SysUtils,
  System.math,
  System.Threading,
  System.DateUtils,
  System.SyncObjs,
  System.Generics.Collections,
  System.JSON,
  Lb.Bybit.SysUtils;

type
  ///<summary>Get Kline - Получение исторических данных</summary>
  TBybitKline = class(TBybitHttpClient)
  private
    FSymbol: String;
    FCategory: TTypeCategory;
    FInterval: TTypeInterval;
    FLimit: Integer;
    FStartTime: Double;
    FEndTime: Double;
    procedure SetCategory(const Value: TTypeCategory);
    procedure SetSymbol(const Value: String);
    procedure SetInterval(const Value: TTypeInterval);
    procedure SetLimit(const Value: Integer);
    procedure SetEndTime(const Value: Double);
    procedure SetStartTime(const Value: Double);
  protected
    FListJson: TJSONArray;
    procedure DoEventMessage(const AMessage: String); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  public {Request Parameters}
    ///<summary>Тип продукта. spot, linear, inverse, option</summary>
    property Category: TTypeCategory read FCategory write SetCategory;
    ///<summary>Имя символа</summary>
    property Symbol: String read FSymbol write SetSymbol;
    ///<summary>Интервал клайна</summary>
    property Interval: TTypeInterval read FInterval write SetInterval;
    property StartTime: Double read FStartTime write SetStartTime;
    property EndTime: Double read FEndTime write SetEndTime;
    ///<summary>Limit for data size per page. [1, 1000]. Default: 200</summary>
    property Limit: Integer read FLimit write SetLimit;
  public
    property ListJson: TJSONArray read FListJson;
  end;

type
  ///<summary>Linear/Inverse</summary>
  TCandelObject = class(TObject)
    startTime: String;
    openPrice: String;
    highPrice: String;
    lowPrice: String;
    closePrice: String;
    volume: String;
    turnover: String;
  private
    function GetDateTime: TDateTime;
  public
    procedure SetObjectJson(const AValueJsons: TJSONArray);
    function ToString: String; override;
    property DateTime: TDateTime read GetDateTime;
  end;

  ///<summary>Массив свечей</summary>
  TCandelObjectList = TObjectList<TCandelObject>;

procedure SetLinearObjects(AListJson: TJSONArray; ACandelObjects: TCandelObjectList);

implementation

procedure SetLinearObjects(AListJson: TJSONArray; ACandelObjects: TCandelObjectList);
var
  i, iCount: Integer;
  xObjectJson: TJSONArray;
  xCandelObject: TCandelObject;
begin
  if not Assigned(AListJson) then
  begin
    raise Exception.Create('Error Message: Объект JSON – не определен');
    Exit;
  end;

  if not Assigned(ACandelObjects) then
  begin
    Exit;
  end;

  ACandelObjects.Clear;
  iCount := AListJson.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xObjectJson := TJSONArray(AListJson.Items[i]);
      xCandelObject := TCandelObject.Create;
      xCandelObject.SetObjectJson(xObjectJson);
      ACandelObjects.Add(xCandelObject);
    end;
end;


{ TBybitKline }

constructor TBybitKline.Create;
begin
  inherited;
  ModuleParam.TypeHttp := TTypeHttp.thGet;
  ModuleParam.Module := '/v5/market/kline';
end;

destructor TBybitKline.Destroy;
begin

  inherited;
end;


procedure TBybitKline.SetCategory(const Value: TTypeCategory);
begin
  FCategory := Value;
  ModuleParam.Params.SetParam('category',GetStrToTypeCategory(FCategory))
end;

procedure TBybitKline.SetSymbol(const Value: String);
begin
  FSymbol := Value;
  ModuleParam.Params.SetParam('symbol',FSymbol);
end;


procedure TBybitKline.SetInterval(const Value: TTypeInterval);
begin
  FInterval := Value;
  ModuleParam.Params.SetParam('interval',GetStrToTypeInterval(FInterval));
end;

procedure TBybitKline.SetStartTime(const Value: Double);
begin
  FStartTime := Value;
  ModuleParam.Params.SetParam('start',FloatToStr(FStartTime));
end;

procedure TBybitKline.SetEndTime(const Value: Double);
begin
  FEndTime := Value;
  ModuleParam.Params.SetParam('end',FloatToStr(FEndTime));
end;

procedure TBybitKline.SetLimit(const Value: Integer);
begin
  FLimit := Value;
  ModuleParam.Params.SetParam('limit',FLimit.ToString);
end;

procedure TBybitKline.DoEventMessage(const AMessage: String);
var
  xValueJson: TJSONValue;
begin
  inherited DoEventMessage(AMessage);
  xValueJson := Response.ResultObject.Values['list'];
  if xValueJson is TJSONArray then
    FListJson := TJSONArray(xValueJson);
end;


{ TCandelObject }

function TCandelObject.GetDateTime: TDateTime;
var
  xValue: TDateTime;
begin
  xValue := UnixToDateTime(
    Round(StrToUInt64Def(Self.StartTime,0)/1000)
  );
  Result := xValue;
end;

procedure TCandelObject.SetObjectJson(const AValueJsons: TJSONArray);
begin
(*
            [
                "1670608800000",
                "17071",
                "17073",
                "17027",
                "17055.5",
                "268611",
                "15.74462667"
            ],
*)
  if (AValueJsons.Count < 7) then
    raise Exception.Create('Error Message: Объект работы');

  startTime  := AValueJsons.Items[0].Value;
  openPrice  := AValueJsons.Items[1].Value;
  highPrice  := AValueJsons.Items[2].Value;
  lowPrice   := AValueJsons.Items[3].Value;
  closePrice := AValueJsons.Items[4].Value;
  volume     := AValueJsons.Items[5].Value;
  turnover   := AValueJsons.Items[6].Value;
end;

function TCandelObject.ToString: String;
begin
  Result :=
    startTime  + ';' +
    openPrice  + ';' +
    highPrice  + ';' +
    lowPrice   + ';' +
    closePrice + ';' +
    volume     + ';' +
    turnover   + ';';
end;

end.
