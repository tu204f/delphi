unit UnitCategoryFrame;

interface

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
    FSide: TTypeSide;
    FTypeTrade: TTypeTrade;
    FTypeLine: TTypeLine;
    FOnEventSendTarde: TOnEventSendTarde;
  protected
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
  end;

implementation

{$R *.fmx}

{ TCategoryFrame }

constructor TCategoryFrame.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TCategoryFrame.Destroy;
begin

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
  ParamApplication.ActiveRSI[TypeTrade,TypeLine] := SpinBoxActiveRSI.Value;
end;

procedure TCategoryFrame.CheckBoxReActiveChange(Sender: TObject);
begin
  ParamApplication.ReActive[TypeTrade,TypeLine] := CheckBoxReActive.IsChecked;
end;

procedure TCategoryFrame.SpinBoxReActiveRSIChange(Sender: TObject);
begin
  ParamApplication.ReActiveRSI[TypeTrade,TypeLine] := SpinBoxReActiveRSI.Value;
end;

procedure TCategoryFrame.SpinBoxQtyChange(Sender: TObject);
begin
  ParamApplication.Qty[TypeTrade,TypeLine] := SpinBoxQty.Value;
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
        if AParam.ValueRSI < SpinBoxActiveRSI.Value then
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
            CheckBoxActive.IsChecked := True;
        end;
      end;
    end;
    tsSell: begin
      if CheckBoxActive.IsChecked then
      begin
        if AParam.ValueRSI > SpinBoxActiveRSI.Value then
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
            CheckBoxActive.IsChecked := True;
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


end.
