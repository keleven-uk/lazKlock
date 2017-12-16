unit uOptionsUtils;
{  Some helper routines for the userOptions class.
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOM, XMLWrite, XMLRead;

Function readChild(PassNode: TDOMNode;  name: string): string;
Function readChildAttribute(PassNode: TDOMNode;  name: string; attribute: string): string;
function writeStrChild(Doc: TXMLDocument; name: string; value: string): TDOMNode;
function writeBolChild(Doc: TXMLDocument; name: string; value: boolean): TDOMNode;
function writeIntChild(Doc: TXMLDocument; name: string; value: integer): TDOMNode;
function writeIntChildAttribute(Doc: TXMLDocument; name: string; value1: integer; value2: integer): TDOMNode;

implementation

uses
  formklock;


Function readChild(PassNode: TDOMNode;  name: string): string;
{  Read a child node and return its [string] value.    }
var
    childNode: TDOMNode;
begin
  childNode := PassNode.FindNode(name);
  result := childNode.TextContent;
end;

Function readChildAttribute(PassNode: TDOMNode;  name: string; attribute: string): string;
{  Read a child node and return its named [string] attribute.    }
var
    childNode: TDOMNode;
    s: string;
begin
  childNode := PassNode.FindNode(name);
  s := TDOMElement(childNode).GetAttribute(attribute);
  result := s;
end;

function writeStrChild(Doc: TXMLDocument; name: string; value: string): TDOMNode;
{  Write a [string] value to a child node.    }
var
  ItemNode, TextNode: TDOMNode;
begin
  ItemNode := Doc.CreateElement(name);
  TextNode := Doc.CreateTextNode(WideString(value));
  ItemNode.AppendChild(TextNode);
  result := ItemNode;
end;

function writeBolChild(Doc: TXMLDocument; name: string; value: boolean): TDOMNode;
{  Write a [boolean] value to a child node.    }
var
  ItemNode, TextNode: TDOMNode;
begin
  ItemNode := Doc.CreateElement(name);
  TextNode := Doc.CreateTextNode(BoolToStr(value));
  ItemNode.AppendChild(TextNode);
  result := ItemNode;
end;

function writeIntChild(Doc: TXMLDocument; name: string; value: integer): TDOMNode;
{  Write a [integer] value to a child node.    }
var
  ItemNode, TextNode: TDOMNode;
begin
  ItemNode := Doc.CreateElement(name);
  TextNode := Doc.CreateTextNode(IntToStr(value));
  ItemNode.AppendChild(TextNode);
  result := ItemNode;
end;

function writeIntChildAttribute(Doc: TXMLDocument; name: string; value1: integer; value2: integer): TDOMNode;
{  Write a [integer] attribute to a child node.

   It seems you have to write both attributes at once, i can read them singularly.
   So, this routine is hard coded for form position until i can fix.
}
var
  ItemNode, TextNode: TDOMNode;
begin
  ItemNode := Doc.CreateElement(name);
  TDOMElement(ItemNode).SetAttribute('Top', IntToStr(value1));
  TDOMElement(ItemNode).SetAttribute('Left', IntToStr(value2));
  TextNode := Doc.CreateTextNode('');
  ItemNode.AppendChild(TextNode);
  result := ItemNode;
end;


end.


