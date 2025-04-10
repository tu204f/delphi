unit UnitWorkBotGrid;

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

  Lb.Bot.Candel,
  System.Rtti,

  FMX.Grid.Style,
  FMX.Controls.Presentation,
  FMX.ScrollBox,
  FMX.Grid;

type
  ///<summary>
  /// ����� ����
  ///</summary>
  TWorkBotGridFrame = class(TFrame)
    WorkBotGrid: TStringGrid;
  private
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.fmx}

uses
  Lb.WorkBot.Grid;

{ TWorkBotGridFrame }

constructor TWorkBotGridFrame.Create(AOwner: TComponent);

  procedure SetShowWorkBotGrid;
  begin
    SetAddColumn(WorkBotGrid,'id',50);
    SetAddColumn(WorkBotGrid,'Count.Position');
    SetAddColumn(WorkBotGrid,'Profit');
    SetAddColumn(WorkBotGrid,'FeeRatesTaker');
    SetAddColumn(WorkBotGrid,'FeeRatesMaker');
  end;

begin
  inherited Create(AOwner);
end;

destructor TWorkBotGridFrame.Destroy;
begin

  inherited;
end;

end.
