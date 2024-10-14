unit UnitCategoryFrame;

interface

{$i platform.inc}

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Controls.Presentation,
  FMX.Edit,
  FMX.EditBox,
  FMX.SpinBox,
  FMX.Layouts,
  FMX.Objects,
  Lb.SysUtils,
  Lb.Level;

type
  ///<summary>Категория совершение торговых операций</summary>
  TCategoryFrame = class(TFrame)
    Rectangle: TRectangle;
    GridPanel: TGridPanelLayout;
    Layout1: TLayout;
    Layout2: TLayout;
    Layout3: TLayout;
    Layout4: TLayout;
    Layout5: TLayout;
    CheckBoxActive: TCheckBox;
    CheckBoxReActive: TCheckBox;
    SpinBoxActiveRSI: TSpinBox;
    SpinBoxReActiveRSI: TSpinBox;
    SpinBoxQty: TSpinBox;
    Layout6: TLayout;
    CheckBoxReversQty: TCheckBox;
    procedure SpinBoxActiveRSIChange(Sender: TObject);
    procedure CheckBoxReActiveChange(Sender: TObject);
    procedure SpinBoxReActiveRSIChange(Sender: TObject);
    procedure SpinBoxQtyChange(Sender: TObject);
    procedure CheckBoxActiveChange(Sender: TObject);
    procedure CheckBoxReversQtyChange(Sender: TObject);
  private
    FSituationParam: TSituationParam;
  private
    FSide: TQBTypeSide;
    FTypeTrade: TTypeTrade;
    FTypeLine: TTypeLine;
    FOnEventSendTarde: TOnEventSendTarde;
    procedure SetSide(const Value: TQBTypeSide);
  protected
    ActiveLevel: TOneEventLevel;
    ReActiveLevel: TOneEventLevel;
    procedure ActiveLevelOnIntersection(Sender: TObject);
    procedure ReActiveLevelOnIntersection(Sender: TObject);
    procedure DoSendTrade(const AParam: TTradeParam);
    procedure SetActiveLevelSide;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Load;
    procedure SetValueParam(AParam: TSituationParam);
    ///<summary>
    /// Напровление котегори
    ///</summary>
    property Side: TQBTypeSide read FSide write SetSide;
    ///<summary>
    /// Направление работы категориями: Long, Short
    ///</summary>
    property TypeTrade: TTypeTrade read FTypeTrade write FTypeTrade;
    property TypeLine: TTypeLine read FTypeLine write FTypeLine;
    property OnEventSendTarde: TOnEventSendTarde write FOnEventSendTarde;
  end;

implementation

{$R *.fmx}

uses
  UnitLogForm,
  Lb.Logger;

{ TCategoryFrame }

constructor TCategoryFrame.Create(AOwner: TComponent);
begin
  inherited;
  ActiveLevel   := TOneEventLevel.Create;
  ActiveLevel.OnIntersectionLevel := ActiveLevelOnIntersection;

  ReActiveLevel := TOneEventLevel.Create;
  ReActiveLevel.OnIntersectionLevel := ReActiveLevelOnIntersection;
end;

destructor TCategoryFrame.Destroy;
begin
  FreeAndNil(ReActiveLevel);
  FreeAndNil(ActiveLevel);
  inherited;
end;


procedure TCategoryFrame.Load;
begin
  CheckBoxActive.IsChecked    := ParamPlatform.Active[TypeTrade,TypeLine];
  CheckBoxReActive.IsChecked  := ParamPlatform.ReActive[TypeTrade,TypeLine];
  SpinBoxActiveRSI.Value      := ParamPlatform.ActiveRSI[TypeTrade,TypeLine];
  SpinBoxReActiveRSI.Value    := ParamPlatform.ReActiveRSI[TypeTrade,TypeLine];
  SpinBoxQty.Value            := ParamPlatform.Qty[TypeTrade,TypeLine];
  CheckBoxReversQty.IsChecked := ParamPlatform.ReversQty[TypeTrade,TypeLine];

  ActiveLevel.Value := SpinBoxActiveRSI.Value;
  ReActiveLevel.Value := SpinBoxReActiveRSI.Value;
end;

procedure TCategoryFrame.CheckBoxActiveChange(Sender: TObject);
begin
  ParamPlatform.Active[TypeTrade,TypeLine] := CheckBoxActive.IsChecked;
end;

procedure TCategoryFrame.SpinBoxActiveRSIChange(Sender: TObject);
begin
  ActiveLevel.Value := SpinBoxActiveRSI.Value;
  ParamPlatform.ActiveRSI[TypeTrade,TypeLine] := SpinBoxActiveRSI.Value;
end;

procedure TCategoryFrame.CheckBoxReActiveChange(Sender: TObject);
begin
  ParamPlatform.ReActive[TypeTrade,TypeLine] := CheckBoxReActive.IsChecked;
end;

procedure TCategoryFrame.CheckBoxReversQtyChange(Sender: TObject);
begin
  ParamPlatform.ReversQty[TypeTrade,TypeLine] := CheckBoxReversQty.IsChecked;
end;

procedure TCategoryFrame.SpinBoxReActiveRSIChange(Sender: TObject);
begin
  ReActiveLevel.Value := SpinBoxReActiveRSI.Value;
  ParamPlatform.ReActiveRSI[TypeTrade,TypeLine] := SpinBoxReActiveRSI.Value;
end;

procedure TCategoryFrame.SpinBoxQtyChange(Sender: TObject);
begin
  ParamPlatform.Qty[TypeTrade,TypeLine] := SpinBoxQty.Value;
end;

procedure TCategoryFrame.ActiveLevelOnIntersection(Sender: TObject);

  function _Qty: Double;
  begin
    if FSituationParam.Qty = 0 then
    begin
      Result := SpinBoxQty.Value;
    end
    else if CheckBoxReversQty.IsChecked then
    begin
      if FSituationParam.Side = FSide then
        Result := SpinBoxQty.Value
      else
        Result := FSituationParam.Qty + SpinBoxQty.Value;
    end
    else
    begin
      Result := FSituationParam.Qty;
    end;
  end;

var
  xParam: TTradeParam;
begin
{$IFDEF DBG_LEVEL_CATEGORY_PARAM}
  TLogger.LogTree(0,'procedure TCategoryFrame.ActiveLevelOnIntersection:');
  TLogger.LogTreeText(3,'>> Status:' + StatusLevelToStr(ActiveLevel.StatusLevel));
  TLogger.LogTreeText(3,'>> Intersection:' + IntersectionLevelToStr(ActiveLevel.IntersectionLevel));
{$ENDIF}
  // Событие пересечение выставить торговый операцию
  if CheckBoxActive.IsChecked then
  begin
    case FSide of
      TQBTypeSide.tsBuy: xParam.Price := FSituationParam.Ask;
      TQBTypeSide.tsSell: xParam.Price := FSituationParam.Bid;
    end;
    xParam.Qty := _Qty;
    xParam.Side:= FSide;
    xParam.TypeTrade := FTypeTrade;
    xParam.TypeLine := FTypeLine;

    DoSendTrade(xParam);
    CheckBoxActive.IsChecked := False;
  end;
end;

procedure TCategoryFrame.ReActiveLevelOnIntersection(Sender: TObject);
begin
{$IFDEF DBG_LEVEL_CATEGORY_PARAM}
  TLogger.LogTree(0,'procedure TCategoryFrame.ReActiveLevelOnIntersection');
  TLogger.LogTreeText(3,'>> Status:' + StatusLevelToStr(ReActiveLevel.StatusLevel));
  TLogger.LogTreeText(3,'>> Intersection:' + IntersectionLevelToStr(ReActiveLevel.IntersectionLevel));
{$ENDIF}
  // Событие пересечение, актировать заявки
  if CheckBoxReActive.IsChecked then
  begin
    CheckBoxActive.IsChecked := True;
    SetActiveLevelSide;
  end;
end;

procedure TCategoryFrame.SetActiveLevelSide;
begin
  case FSide of
    TQBTypeSide.tsBuy: begin
      ActiveLevel.IsRepeat := False;
      ActiveLevel.WorkLevel := TIntersectionLevel.tlDownUp;

      ReActiveLevel.IsRepeat := False;
      ReActiveLevel.WorkLevel := TIntersectionLevel.tlUpDown;
    end;
    TQBTypeSide.tsSell: begin
      ActiveLevel.IsRepeat := False;
      ActiveLevel.WorkLevel := TIntersectionLevel.tlUpDown;

      ReActiveLevel.IsRepeat := False;
      ReActiveLevel.WorkLevel := TIntersectionLevel.tlDownUp;
    end;
  end;
end;

procedure TCategoryFrame.SetSide(const Value: TQBTypeSide);
begin
  FSide := Value;
  case FSide of
    TQBTypeSide.tsBuy : begin
      Self.SetActiveLevelSide;
      Rectangle.Fill.Color := TAlphaColorRec.Green;
    end;
    TQBTypeSide.tsSell: begin
      Self.SetActiveLevelSide;
      Rectangle.Fill.Color := TAlphaColorRec.Red;
    end;
  else
    Rectangle.Fill.Color := TAlphaColorRec.Null;
  end;
end;

procedure TCategoryFrame.SetValueParam(AParam: TSituationParam);

  procedure _ActiveLevelSetUpDate(const AValue: Double);
  begin
{$IFDEF DBG_LEVEL_CATEGORY_PARAM}
    TLogger.LogTree(0,'procedure TCategoryFrame.SetValueParam._ActiveLevelSetUpDate' +
      'Current:' + AValue.ToString + ' ' +
      'Value:' + ActiveLevel.Value.ToString
    );
{$ENDIF}
    if CheckBoxActive.IsChecked then
      ActiveLevel.SetUpDate(AValue);
  end;

  procedure _ReActiveLevelSetUpDate(const AValue: Double);
  begin
{$IFDEF DBG_LEVEL_CATEGORY_PARAM}
    TLogger.LogTree(0,'procedure TCategoryFrame.SetValueParam._ReActiveLevelSetUpDate' +
      'Current:' + AValue.ToString + ' ' +
      'Value:' + ReActiveLevel.Value.ToString
    );
{$ENDIF}
    if not CheckBoxActive.IsChecked and CheckBoxReActive.IsChecked then
      ReActiveLevel.SetUpDate(AValue);
  end;

  procedure _SetValueParam(AParam: TSituationParam);
  begin
{$IFDEF DBG_LEVEL_CATEGORY_PARAM}
    TLogger.LogTree(0,'procedure TCategoryFrame.SetValueParam._SetValueParam');
{$ENDIF}
    if ParamPlatform.IsTrend then
    begin
      case FSide of
        // Покупаем если рынок растет
        TQBTypeSide.tsBuy: begin
          if AParam.SlowRSI > VALUE_TRADE_RSI then
            _ActiveLevelSetUpDate(AParam.FastRSI);
        end;
        // Продаем если рынок падает
        TQBTypeSide.tsSell: begin
          if AParam.SlowRSI < VALUE_TRADE_RSI then
            _ActiveLevelSetUpDate(AParam.FastRSI);
        end;
      end;
      _ReActiveLevelSetUpDate(AParam.FastRSI);
    end
    else
    begin
      _ActiveLevelSetUpDate(AParam.FastRSI);
      _ReActiveLevelSetUpDate(AParam.FastRSI);
    end;
  end;

begin
{$IFDEF DBG_LEVEL}
  TLogger.LogTree(0,'TCategoryFrame.SetValueParam: Параметры уровня(' +
    'Side :' + GetStrToTypeSide(FSide) + '; ' +
    'Trade:' + GetStrToTypeTrade(FTypeTrade) + '; ' +
    'Line :' + GetStrToTypeLine(FTypeLine)
   + ')');
  TLogger.LogTree(0,'>> CATEGORY_FRAME');
{$ENDIF}
  FSituationParam := AParam;
  _SetValueParam(AParam);
end;

procedure TCategoryFrame.DoSendTrade(const AParam: TTradeParam);
begin
  if Assigned(FOnEventSendTarde) then
    FOnEventSendTarde(Self,AParam);
end;


end.
