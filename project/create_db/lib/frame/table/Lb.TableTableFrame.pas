unit Lb.TableTableFrame;

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
  System.Rtti,
  FMX.Grid.Style,
  FMX.ScrollBox,
  FMX.Grid,
  FMX.Controls.Presentation,
  FMX.Layouts;

type
  TTableTableFrame = class(TFrame)
    Layout: TLayout;
    LayoutMenu: TLayout;
    ButtonAdd: TButton;
    ButtonEdit: TButton;
    ButtonDelete: TButton;
    StringGrid: TStringGrid;
  private
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.fmx}

{ TTablesFrame }

constructor TTableTableFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

end;

destructor TTableTableFrame.Destroy;
begin

  inherited;
end;

end.
