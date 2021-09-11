unit Lb.MainFrame;

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
  FMX.TabControl,
  Lb.DomainTableFrame;

type
  TMainFrame = class(TFrame)
    TabControl: TTabControl;
    TabItemDomain: TTabItem;
  private
  public
    DomainTableFrame: TDomainTableFrame;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.fmx}

{ TMainFrame }

constructor TMainFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  DomainTableFrame := TDomainTableFrame.Create(Self);
  DomainTableFrame.Parent := TabItemDomain;
  DomainTableFrame.Align := TAlignLayout.Client;

end;

destructor TMainFrame.Destroy;
begin

  FreeAndNil(DomainTableFrame);
  inherited;
end;

end.
