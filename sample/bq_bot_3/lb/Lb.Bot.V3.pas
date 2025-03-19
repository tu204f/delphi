unit Lb.Bot.V3;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  Lb.SysUtils,
  Lb.Breakdown;

type
  TWorkBotDeviation = class;

  ///<summary>������ ��������<summary>
  TWorkBotStatus = (
    wbsNull,  // ������ �� ���������
    wbsHigh,  // ������� ������� ������� �������������
    wbsLow    // ������� ������ ������� �������������
  );

  IEventWorkBot = interface
    procedure EventNewCandel;
    procedure EventParamValue(const AWorkBot: TWorkBotDeviation);
    procedure Event�rossingLevel(const AWorkBotStatus: TWorkBotStatus);
  end;
  TOnEvent�rossingLevel = procedure(Sender: TObject; const AWorkBotStatus: TWorkBotStatus) of object;

  ///<summary>��������� ���������</summary>
  TWorkBotDeviation = class(TObject)
  public type
    TParam = record
      Candel: TCandel;
      Deviation: Double;
      Rate: Double;
    end;
  private
    FCountParamValue: Integer;
    FPriceHigh: Double;
    FPriceLow: Double;
    FParam: TWorkBotDeviation.TParam;
    FEventWorkBot: IEventWorkBot;
    FWorkBotStatus: TWorkBotStatus;
    function GetCandel: TCandel;
    function GetDeviation: Double;
  protected
    FOn�rossingHigh: TNotifyEvent;
    FOn�rossingLow: TNotifyEvent;
    FOn�rossingLevel: TOnEvent�rossingLevel;
    procedure Do�rossingHigh;
    procedure Do�rossingLow;
    procedure Do�rossingLevel(const AWorkBotStatus: TWorkBotStatus);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SetNewCandel;
    procedure SetParamValue(const AParam: TWorkBotDeviation.TParam);
    property CountParamValue: Integer read FCountParamValue;
    property PriceHigh: Double read FPriceHigh;
    property PriceLow: Double read FPriceLow;

    property On�rossingHigh: TNotifyEvent write FOn�rossingHigh;
    property On�rossingLow: TNotifyEvent write FOn�rossingLow;
    property On�rossingLevel: TOnEvent�rossingLevel write FOn�rossingLevel;
  public
    property Candel: TCandel read GetCandel;
    property Deviation: Double read GetDeviation;
    property EventWorkBot: IEventWorkBot write FEventWorkBot;
    property WorkBotStatus: TWorkBotStatus read FWorkBotStatus;
  end;

implementation

uses
  Lb.Logger,
  System.DateUtils;

{ TWorkBotDeviation }

constructor TWorkBotDeviation.Create;
begin
  FCountParamValue := 0;
  FEventWorkBot := nil;
  FWorkBotStatus := TWorkBotStatus.wbsNull;
end;

destructor TWorkBotDeviation.Destroy;
begin

  inherited;
end;

procedure TWorkBotDeviation.SetNewCandel;
begin
  FCountParamValue := 0;
  if Assigned(FEventWorkBot) then
    FEventWorkBot.EventNewCandel;
end;

procedure TWorkBotDeviation.SetParamValue(const AParam: TWorkBotDeviation.TParam);
begin
  Inc(FCountParamValue);
  FParam := AParam;
  with AParam do
  begin
    FPriceHigh := Candel.Open + Deviation * Rate;
    FPriceLow  := Candel.Open - Deviation * Rate;
    if FPriceHigh < Candel.High then
      Do�rossingHigh
    else if FPriceLow > Candel.Low then
      Do�rossingLow;
  end;
  if Assigned(FEventWorkBot) then
    FEventWorkBot.EventParamValue(Self);
end;

procedure TWorkBotDeviation.Do�rossingLevel(const AWorkBotStatus: TWorkBotStatus);
begin
  {$IFDEF DEBUG}
  TLogger.LogTree(0,'TWorkBotDeviation.Do�rossingLevel: ����������� ������ �� �������');
  {$ENDIF}
  if Assigned(FOn�rossingLevel) then
    FOn�rossingLevel(Self,AWorkBotStatus);
end;

procedure TWorkBotDeviation.Do�rossingHigh;
begin
  if Assigned(FOn�rossingHigh) then
    FOn�rossingHigh(Self);

  if (FWorkBotStatus in [TWorkBotStatus.wbsNull,TWorkBotStatus.wbsLow]) then
  begin
    FWorkBotStatus := TWorkBotStatus.wbsHigh;

    if Assigned(FEventWorkBot) then
      FEventWorkBot.Event�rossingLevel(FWorkBotStatus);

    Do�rossingLevel(FWorkBotStatus);
  end;
end;

procedure TWorkBotDeviation.Do�rossingLow;
begin
  if Assigned(FOn�rossingLow) then
    FOn�rossingLow(Self);

  if (FWorkBotStatus in [TWorkBotStatus.wbsNull,TWorkBotStatus.wbsHigh]) then
  begin
    FWorkBotStatus := TWorkBotStatus.wbsLow;

    if Assigned(FEventWorkBot) then
      FEventWorkBot.Event�rossingLevel(FWorkBotStatus);

    Do�rossingLevel(FWorkBotStatus);
  end;
end;

function TWorkBotDeviation.GetCandel: TCandel;
begin
  Result := FParam.Candel;
end;

function TWorkBotDeviation.GetDeviation: Double;
begin
  Result := FParam.Deviation;
end;

end.
