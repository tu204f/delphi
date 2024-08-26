unit UnitCategoryFrame;

interface

{$I debug.inc}

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
  Lb.Level,
  Lb.Bybit.SysUtils;

type
  ///<summary> атегори€ совершение торговых операций</summary>
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
    procedure SpinBoxActiveRSIChange(Sender: TObject);
    procedure CheckBoxReActiveChange(Sender: TObject);
    procedure SpinBoxReActiveRSIChange(Sender: TObject);
    procedure SpinBoxQtyChange(Sender: TObject);
    procedure CheckBoxActiveChange(Sender: TObject);
  private
    FIsBoxActive: Boolean;
    function GetActiveValueRSI(const ADeltaSize: Integer = 3): Double;
  private
    FSide: TTypeSide;
    FTypeTrade: TTypeTrade;
    FTypeLine: TTypeLine;
    FOnEventSendTarde: TOnEventSendTarde;
    procedure SetColor(const Value: TAlphaColor);
  protected
    ActiveLevel: TOneEventLevel;
    ReActiveLevel: TOneEventLevel;
    procedure ActiveIntersectionLevel(Sender: TObject);
    procedure ReActiveIntersectionLevel(Sender: TObject);
    procedure DoSendTrade(const AParam: TTradeParam);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Load;
    procedure SetValueParam(AParam: TSituationParam);
    property Side: TTypeSide read FSide write FSide;
    property TypeTrade: TTypeTrade read FTypeTrade write FTypeTrade;
    property TypeLine: TTypeLine read FTypeLine write FTypeLine;
    property OnEventSendTarde: TOnEventSendTarde write FOnEventSendTarde;
    property Color: TAlphaColor write SetColor;
  end;

implementation

{$R *.fmx}

uses
  Lb.Logger;

{ TCategoryFrame }

constructor TCategoryFrame.Create(AOwner: TComponent);
begin
  inherited;
  ActiveLevel := TOneEventLevel.Create;
  ActiveLevel.IsRepeat := False;
  ActiveLevel.OnIntersectionLevel := ActiveIntersectionLevel;

  ReActiveLevel := TOneEventLevel.Create;
  ReActiveLevel.IsRepeat := False;
  ReActiveLevel.OnIntersectionLevel := ReActiveIntersectionLevel;

end;

destructor TCategoryFrame.Destroy;
begin
  FreeAndNil(ReActiveLevel);
  FreeAndNil(ActiveLevel);
  inherited;
end;


procedure TCategoryFrame.Load;
begin
  CheckBoxActive.IsChecked   := ParamApplication.Active[TypeTrade,TypeLine];
  CheckBoxReActive.IsChecked := ParamApplication.ReActive[TypeTrade,TypeLine];
  SpinBoxActiveRSI.Value     := ParamApplication.ActiveRSI[TypeTrade,TypeLine];
  SpinBoxReActiveRSI.Value   := ParamApplication.ReActiveRSI[TypeTrade,TypeLine];
  SpinBoxQty.Value           := ParamApplication.Qty[TypeTrade,TypeLine];
end;

procedure TCategoryFrame.CheckBoxActiveChange(Sender: TObject);
begin
  ParamApplication.Active[TypeTrade,TypeLine] := CheckBoxActive.IsChecked;
end;

procedure TCategoryFrame.SpinBoxActiveRSIChange(Sender: TObject);
begin
  ActiveLevel.Value := SpinBoxActiveRSI.Value;
  ParamApplication.ActiveRSI[TypeTrade,TypeLine] := SpinBoxActiveRSI.Value;
end;

procedure TCategoryFrame.CheckBoxReActiveChange(Sender: TObject);
begin
  ParamApplication.ReActive[TypeTrade,TypeLine] := CheckBoxReActive.IsChecked;
end;

procedure TCategoryFrame.SpinBoxReActiveRSIChange(Sender: TObject);
begin
  ReActiveLevel.Value := SpinBoxReActiveRSI.Value;
  ParamApplication.ReActiveRSI[TypeTrade,TypeLine] := SpinBoxReActiveRSI.Value;
end;

procedure TCategoryFrame.SpinBoxQtyChange(Sender: TObject);
begin
  ParamApplication.Qty[TypeTrade,TypeLine] := SpinBoxQty.Value;
end;

procedure TCategoryFrame.SetColor(const Value: TAlphaColor);
begin
  Rectangle.Fill.Color := Value;
end;

procedure TCategoryFrame.SetValueParam(AParam: TSituationParam);

  function _Qty(AParam: TSituationParam): Double;
  begin
    if SpinBoxQty.Value > 0 then
      Result := SpinBoxQty.Value
    else
      Result := AParam.Qty;
  end;

var
  xParam: TTradeParam;
begin

  case FSide of

    tsBuy: begin
      if CheckBoxActive.IsChecked then
      begin

        if not FIsBoxActive then
          FIsBoxActive := AParam.ValueRSI < GetActiveValueRSI;

        if FIsBoxActive and (AParam.ValueRSI > SpinBoxActiveRSI.Value) then
        begin
          xParam.Price := AParam.Ask;
          xParam.Qty   := _Qty(AParam);
          xParam.Side  := FSide;
          xParam.TypeLine := FTypeLine;
          xParam.TypeTrade := FTypeTrade;
          DoSendTrade(xParam);
          CheckBoxActive.IsChecked := False;
        end;
      end
      else
      begin
        if CheckBoxReActive.IsChecked then
        begin
          if AParam.ValueRSI > SpinBoxReActiveRSI.Value then
          begin
            FIsBoxActive := False;
            CheckBoxActive.IsChecked := True;
          end;
        end;
      end;
    end;

    tsSell: begin
      if CheckBoxActive.IsChecked then
      begin
        if not FIsBoxActive then
          FIsBoxActive := AParam.ValueRSI > GetActiveValueRSI;

        if FIsBoxActive and (AParam.ValueRSI < SpinBoxActiveRSI.Value) then
        begin
          xParam.Price := AParam.Ask;
          xParam.Qty   := _Qty(AParam);
          xParam.Side  := FSide;
          xParam.TypeLine := FTypeLine;
          xParam.TypeTrade := FTypeTrade;
          DoSendTrade(xParam);
          CheckBoxActive.IsChecked := False;
        end;
      end
      else
      begin
        if CheckBoxReActive.IsChecked then
        begin
          if AParam.ValueRSI < SpinBoxReActiveRSI.Value then
          begin
            CheckBoxActive.IsChecked := True;
            FIsBoxActive := False;
          end;
        end;
      end;
    end;

  end;
end;

procedure TCategoryFrame.DoSendTrade(const AParam: TTradeParam);
begin
  if Assigned(FOnEventSendTarde) then
    FOnEventSendTarde(Self,AParam);
end;

function TCategoryFrame.GetActiveValueRSI(const ADeltaSize: Integer): Double;
begin
  case FSide of
    tsBuy: Result := SpinBoxActiveRSI.Value - ADeltaSize;
    tsSell: Result := SpinBoxActiveRSI.Value - ADeltaSize;
  else
    Result := SpinBoxActiveRSI.Value;
  end;
end;

procedure TCategoryFrame.ActiveIntersectionLevel(Sender: TObject);
begin

end;

procedure TCategoryFrame.ReActiveIntersectionLevel(Sender: TObject);
begin

end;

end.
