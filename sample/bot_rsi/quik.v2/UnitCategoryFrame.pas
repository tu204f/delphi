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
    /// Ќапровление котегори
    ///</summary>
    property Side: TQBTypeSide read FSide write SetSide;
    ///<summary>
    /// Ќаправление работы категори€ми: Long, Short
    ///</summary>
    property TypeTrade: TTypeTrade read FTypeTrade write FTypeTrade;
    property TypeLine: TTypeLine read FTypeLine write FTypeLine;
    property OnEventSendTarde: TOnEventSendTarde write FOnEventSendTarde;
  end;

implementation

{$R *.fmx}

uses
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
  CheckBoxActive.IsChecked    := ParamApplication.Active[TypeTrade,TypeLine];
  CheckBoxReActive.IsChecked  := ParamApplication.ReActive[TypeTrade,TypeLine];
  SpinBoxActiveRSI.Value      := ParamApplication.ActiveRSI[TypeTrade,TypeLine];
  SpinBoxReActiveRSI.Value    := ParamApplication.ReActiveRSI[TypeTrade,TypeLine];
  SpinBoxQty.Value            := ParamApplication.Qty[TypeTrade,TypeLine];
  CheckBoxReversQty.IsChecked := ParamApplication.ReversQty[TypeTrade,TypeLine];

  ActiveLevel.Value := SpinBoxActiveRSI.Value;
  ReActiveLevel.Value := SpinBoxReActiveRSI.Value;
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

procedure TCategoryFrame.CheckBoxReversQtyChange(Sender: TObject);
begin
  ParamApplication.ReversQty[TypeTrade,TypeLine] := CheckBoxReversQty.IsChecked;
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

procedure TCategoryFrame.ActiveLevelOnIntersection(Sender: TObject);

  function _Qty: Double;
  begin
    if SpinBoxQty.Value = 0 then
    begin
      Result := Abs(FSituationParam.Qty);
    end
    else if CheckBoxReversQty.IsChecked then
    begin
      Result := Abs(FSituationParam.Qty) + SpinBoxQty.Value;
    end
    else
    begin
      if FTypeLine = tlClose then
        Result := Abs(FSituationParam.Qty)
      else
        Result := SpinBoxQty.Value;
    end;
  end;

var
  xParam: TTradeParam;
begin
  // —обытие пересечение выставить торговый операцию
  if CheckBoxActive.IsChecked then
  begin
    case FSide of
      tsBuy: xParam.Price := FSituationParam.Ask;
      tsSell: xParam.Price := FSituationParam.Bid;
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
  // —обытие пересечение, актировать за€вки
  if CheckBoxReActive.IsChecked then
  begin
    CheckBoxActive.IsChecked := True;
    SetActiveLevelSide;
  end;
end;

procedure TCategoryFrame.SetActiveLevelSide;
begin
  case FSide of
    tsBuy: begin
      ActiveLevel.IsRepeat := False;
      ActiveLevel.WorkLevel := TIntersectionLevel.tlDownUp;
      ReActiveLevel.IsRepeat := False;
      ReActiveLevel.WorkLevel := TIntersectionLevel.tlUpDown;
    end;
    tsSell: begin
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
    tsBuy : begin
      Self.SetActiveLevelSide;
      Rectangle.Fill.Color := TAlphaColorRec.Green;
    end;
    tsSell: begin
      Self.SetActiveLevelSide;
      Rectangle.Fill.Color := TAlphaColorRec.Red;
    end;
  else
    Rectangle.Fill.Color := TAlphaColorRec.Null;
  end;
end;

procedure TCategoryFrame.SetValueParam(AParam: TSituationParam);
begin
  FSituationParam := AParam;
  ActiveLevel.SetUpDate(AParam.ValueRSI);
  ReActiveLevel.SetUpDate(AParam.ValueRSI);
end;

procedure TCategoryFrame.DoSendTrade(const AParam: TTradeParam);
begin
  if Assigned(FOnEventSendTarde) then
    FOnEventSendTarde(Self,AParam);
end;


end.
