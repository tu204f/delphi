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
  TCandelObject = class;
  TCandelObjectList = TObjectList<TCandelObject>;

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
    FCandelObjects: TCandelObjectList;
    procedure DoEventParser; override;
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
    property CandelObjects: TCandelObjectList read FCandelObjects;
  end;

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
  BybitModule.TypeHttp := TTypeHttp.thGet;
  BybitModule.Module := '/v5/market/kline';
  FCandelObjects := TCandelObjectList.Create;
end;

destructor TBybitKline.Destroy;
begin
  FreeAndNil(FCandelObjects);
  inherited;
end;


procedure TBybitKline.SetCategory(const Value: TTypeCategory);
begin
  FCategory := Value;
  BybitModule.Params.SetParam('category',GetStrToTypeCategory(FCategory))
end;

procedure TBybitKline.SetSymbol(const Value: String);
begin
  FSymbol := Value;
  BybitModule.Params.SetParam('symbol',FSymbol);
end;


procedure TBybitKline.SetInterval(const Value: TTypeInterval);
begin
  FInterval := Value;
  BybitModule.Params.SetParam('interval',GetStrToTypeInterval(FInterval));
end;

procedure TBybitKline.SetStartTime(const Value: Double);
begin
  FStartTime := Value;
  BybitModule.Params.SetParam('start',FloatToStr(FStartTime));
end;

procedure TBybitKline.SetEndTime(const Value: Double);
begin
  FEndTime := Value;
  BybitModule.Params.SetParam('end',FloatToStr(FEndTime));
end;

procedure TBybitKline.SetLimit(const Value: Integer);
begin
  FLimit := Value;
  BybitModule.Params.SetParam('limit',FLimit.ToString);
end;

procedure TBybitKline.DoEventParser;
var
  xValueJson: TJSONValue;
begin
  xValueJson := Response.ResultObject.Values['list'];
  if xValueJson is TJSONArray then
  begin
    FListJson := TJSONArray(xValueJson);
    SetLinearObjects(FListJson,FCandelObjects);
  end;
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
  if (AValueJsons.Count < 7) then
    raise Exception.Create('Error Message: Объект работы');
  startTime  := GetStrToJson(AValueJsons.Items[0]);
  openPrice  := GetStrToJson(AValueJsons.Items[1]);
  highPrice  := GetStrToJson(AValueJsons.Items[2]);
  lowPrice   := GetStrToJson(AValueJsons.Items[3]);
  closePrice := GetStrToJson(AValueJsons.Items[4]);
  volume     := GetStrToJson(AValueJsons.Items[5]);
  turnover   := GetStrToJson(AValueJsons.Items[6]);
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
