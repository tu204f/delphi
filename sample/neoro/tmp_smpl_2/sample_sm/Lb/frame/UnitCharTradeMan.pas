(******************************************************************************)
(* Визуально показать действия трейдора                                       *)
(******************************************************************************)
unit UnitCharTradeMan;

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
  FMX.Layouts,
  UnitChartFrame;

type
  TCharTradeManFrame = class(TFrame)
    LayoutPosition: TLayout;
    LayoutTrade: TLayout;
  private
  protected
    Chart: TChartFrame;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.fmx}

{ TCharTradeManFrame }

constructor TCharTradeManFrame.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TCharTradeManFrame.Destroy;
begin

  inherited;
end;

end.
