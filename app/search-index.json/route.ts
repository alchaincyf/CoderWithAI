import { buildIndex } from "@/lib/index-data";

export const dynamic = "force-static";

export function GET() {
  return Response.json(buildIndex());
}
