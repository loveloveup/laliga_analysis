/* =============================================================================
 *  fbx.js — サッカー分析レポートのインタラクティブエンジン（Shiny 不要）
 *
 *  - R 側が window.FBX_DATA（列指向 JSON）を埋め込み、各章は FBX.mount(名前, id) を呼ぶだけ。
 *  - シーズンと節の範囲は FBX が一元管理し、どこで変えても全章が再描画される。
 *  - 章ごとの選択（チーム・選手・指標など）は章の中だけで持つ。
 *  - グラフは SVG を文字列で組み立てる。外部ライブラリは使わない。
 * ========================================================================== */
window.FBX = (() => {
  'use strict';

  // ===========================================================================
  // 0. データ展開
  // ===========================================================================
  const D = window.FBX_DATA || {};
  const A = x => (x == null ? [] : Array.isArray(x) ? x : [x]);
  const one = x => (Array.isArray(x) ? x[0] : x);
  const isNum = v => typeof v === 'number' && isFinite(v);

  /** 列指向 {a:[..], b:[..]} → 行オブジェクトの配列 */
  function rowsOf(obj) {
    if (!obj || typeof obj !== 'object' || Array.isArray(obj)) return [];
    const ks = Object.keys(obj), cols = ks.map(k => A(obj[k]));
    const n = cols.length ? cols[0].length : 0, out = new Array(n);
    for (let i = 0; i < n; i++) {
      const r = {};
      for (let j = 0; j < ks.length; j++) r[ks[j]] = cols[j][i];
      out[i] = r;
    }
    return out;
  }

  const META = D.meta || {};
  const SEASONS = A(META.seasons), SLAB = A(META.seasonLabels);
  const LEAGUES = A(META.leagues), LEAGUE_LAB = A(META.leagueLabels);
  const LG_I = LEAGUES.indexOf(one(META.league));
  const TEAMS = A(D.teams), PLAYERS = A(D.players), TEAMS_ALL = A(D.teamsAll);
  const SQUADS = A(D.squads), FB_PLAYERS = A(D.fbPlayers), POSITIONS = A(D.positions);
  const RESULTS = A(D.shotResults), SITUATIONS = A(D.situations), METHODS = A(D.methods);
  const FOCUS_T = TEAMS.indexOf(one(META.focusTeam));
  const FOCUS_P = A(META.focusPlayers).map(n => PLAYERS.indexOf(n)).filter(i => i >= 0);
  const DEFAULT_S = Math.max(0, one(META.defaultSeason) || 0);

  const race = rowsOf(D.race), shots = rowsOf(D.shots), trend = rowsOf(D.trend);
  const tplayers = rowsOf(D.teamPlayers), offT = rowsOf(D.offTeam), offP = rowsOf(D.offPlayer);
  const tmap = rowsOf(D.teamMap);

  const JA = {
    OpenPlay: '流れの中', FromCorner: 'コーナーキック', SetPiece: 'セットプレー',
    DirectFreekick: '直接FK', Penalty: 'PK',
    Goal: 'ゴール', SavedShot: 'セーブされた', MissedShots: '枠外', BlockedShot: 'ブロック',
    ShotOnPost: 'ポスト', OwnGoal: 'オウンゴール'
  };
  const ja = s => JA[s] || s;
  const seasonLabelOf = y => y + '/' + String((y + 1) % 100).padStart(2, '0');

  // ---- 試合（チーム×試合）----
  race.forEach(r => {
    r.xp = isNum(r.xp) ? r.xp : 0;
    r.pt = r.g > r.ga ? 3 : r.g === r.ga ? 1 : 0;
    r.res = r.g > r.ga ? 'W' : r.g === r.ga ? 'D' : 'L';
  });
  const mdKey = {};
  race.forEach(r => { mdKey[r.s + '|' + r.mid + '|' + r.t] = r.n; });

  // ---- シュート：チームの第何節か、結果・状況のフラグ ----
  shots.forEach(x => {
    x.n = mdKey[x.s + '|' + x.mid + '|' + x.t];
    x.res = RESULTS[x.r];
    x.goal = x.res === 'Goal';
    x.og = x.res === 'OwnGoal';
    x.onT = x.res === 'Goal' || x.res === 'SavedShot';
    x.pen = SITUATIONS[x.si] === 'Penalty';
  });

  const cache = {};
  const memo = (key, fn) => (key in cache ? cache[key] : (cache[key] = fn()));
  const seasonRace = s => memo('race' + s, () => race.filter(r => r.s === s));
  const seasonShots = s => memo('shots' + s, () => shots.filter(x => x.s === s && !x.og));
  const maxMd = s => memo('max' + s, () => seasonRace(s).reduce((m, r) => Math.max(m, r.n), 0));
  const teamCount = s => memo('tc' + s, () => new Set(seasonRace(s).map(r => r.t)).size);

  // ===========================================================================
  // 1. 表示ヘルパー
  // ===========================================================================
  const nf0 = new Intl.NumberFormat('ja-JP', { maximumFractionDigits: 0 });
  const fx = {
    int: v => (isNum(v) ? nf0.format(Math.round(v)) : '—'),
    d1: v => (isNum(v) ? v.toFixed(1) : '—'),
    d2: v => (isNum(v) ? v.toFixed(2) : '—'),
    d3: v => (isNum(v) ? v.toFixed(3) : '—'),
    s0: v => (isNum(v) ? (v > 0 ? '+' : '') + Math.round(v) : '—'),
    s1: v => (isNum(v) ? (v > 0 ? '+' : '') + v.toFixed(1) : '—'),
    s2: v => (isNum(v) ? (v > 0 ? '+' : '') + v.toFixed(2) : '—'),
    pct: v => (isNum(v) ? (v * 100).toFixed(1) + '%' : '—')
  };
  const fmtTick = v => (Math.abs(v) >= 1000 ? nf0.format(v) : String(+v.toFixed(3)));
  const esc = s => String(s == null ? '' : s)
    .replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')
    .replace(/"/g, '&quot;').replace(/'/g, '&#39;');
  const r1 = v => Math.round(v * 10) / 10;
  const textW = (s, fs) => { let w = 0; for (const ch of String(s)) w += ch.charCodeAt(0) > 255 ? fs : fs * 0.58; return w; };
  const trunc = (s, n) => (String(s).length > n ? String(s).slice(0, n - 1) + '…' : String(s));
  const norm = s => String(s || '').normalize('NFD').replace(/[̀-ͯ]/g, '').toLowerCase();
  const median = arr => {
    const a = arr.filter(isNum).sort((x, y) => x - y);
    if (!a.length) return null;
    const h = Math.floor(a.length / 2);
    return a.length % 2 ? a[h] : (a[h - 1] + a[h]) / 2;
  };
  const div = (a, b) => (b > 0 ? a / b : null);

  // 検証済みカテゴリ8色（この順で割り当て、循環させない）
  const SERIES = ['#2a78d6', '#eb6834', '#1baf7a', '#eda100', '#e87ba4', '#008300', '#4a3aa7', '#e34948'];
  const MUTED = '#c3c2b7';      // 背景として描く系列
  const EMPH = '#52514e';       // 検索で一致した（色枠外の）強調
  const SURFACE = '#fcfcfb';

  /** 系列色の割り当て：選んだ順に空いている枠を使い、外すまで同じ色を保つ */
  class Pool {
    constructor(max = 8) { this.m = new Map(); this.max = max; }
    has(id) { return this.m.has(id); }
    color(id) { return this.m.has(id) ? SERIES[this.m.get(id)] : null; }
    add(id) {
      if (this.m.has(id)) return true;
      const used = new Set(this.m.values());
      for (let i = 0; i < this.max; i++) if (!used.has(i)) { this.m.set(id, i); return true; }
      return false;
    }
    remove(id) { this.m.delete(id); }
    clear() { this.m.clear(); }
    ids() { return [...this.m.keys()]; }
    get size() { return this.m.size; }
  }

  const row = (lab, inner, cls) =>
    `<div class="fbx-row${cls ? ' ' + cls : ''}">${lab ? `<span class="fbx-lab">${lab}</span>` : ''}${inner}</div>`;
  const lab = (t, gap) => `<span class="fbx-lab${gap ? ' gap' : ''}">${t}</span>`;
  const selectHTML = (cls, items, sel) =>
    `<select class="${cls}">` +
    items.map(it => `<option value="${esc(it[0])}"${String(it[0]) === String(sel) ? ' selected' : ''}>${esc(it[1])}</option>`).join('') +
    '</select>';
  const chk = (cls, text, on, attrs) =>
    `<label class="fbx-chk"><input type="checkbox" class="${cls}"${attrs || ''}${on ? ' checked' : ''}>${text}</label>`;
  const note = t => `<div class="fbx-note">${t}</div>`;
  const notice = t => `<div class="fbx-notice">${t}</div>`;

  function kpis(list) {
    return '<div class="fbx-kpis">' + list.map(x =>
      `<div class="fbx-kpi${x.main ? ' main' : ''}"><div class="k">${esc(x.k)}</div><div class="v">${esc(x.v)}</div>` +
      (x.s ? `<div class="s">${esc(x.s)}</div>` : '') + '</div>').join('') + '</div>';
  }

  function legendHTML(items) {
    return '<div class="fbx-legend">' + items.map(x =>
      `<span><i class="${x.shape || ''}" style="background:${x.color}"></i>${esc(x.nm)}</span>`).join('') + '</div>';
  }

  function hbars(items, color) {
    const mx = Math.max(1e-9, ...items.map(x => Math.abs(x.v)));
    return '<div class="fbx-hbars">' + items.map(x =>
      `<div class="fbx-hrow"><div class="fbx-hlab" title="${esc(x.nm)}">${esc(x.nm)}</div>` +
      `<div class="fbx-htrack"><div class="fbx-hfill" style="width:${(Math.max(0, x.v) / mx * 100).toFixed(2)}%;background:${x.color || color || SERIES[0]}"></div></div>` +
      `<div class="fbx-hval">${esc(x.txt)}</div></div>`).join('') + '</div>';
  }

  // ===========================================================================
  // 2. ツールチップ（textContent で組み立てる）
  // ===========================================================================
  let tipEl = null;
  function tipShow(ev, t) {
    if (!t) { tipHide(); return; }
    if (!tipEl) { tipEl = document.createElement('div'); tipEl.className = 'fbx-tip'; document.body.appendChild(tipEl); }
    tipEl.textContent = '';
    if (t.title) {
      const h = document.createElement('div'); h.className = 'fbx-tip-h'; h.textContent = t.title; tipEl.appendChild(h);
    }
    (t.rows || []).forEach(r => {
      const d = document.createElement('div'); d.className = 'fbx-tip-r';
      if (r.color) { const k = document.createElement('i'); k.style.background = r.color; d.appendChild(k); }
      const b = document.createElement('b'); b.textContent = r.v; d.appendChild(b);
      if (r.k) { const s = document.createElement('span'); s.textContent = r.k; d.appendChild(s); }
      tipEl.appendChild(d);
    });
    tipEl.style.display = 'block';
    const w = tipEl.offsetWidth, h = tipEl.offsetHeight;
    let x = ev.clientX + 14, y = ev.clientY + 14;
    if (x + w > window.innerWidth - 8) x = ev.clientX - w - 14;
    if (y + h > window.innerHeight - 8) y = ev.clientY - h - 14;
    tipEl.style.left = Math.max(4, x) + 'px';
    tipEl.style.top = Math.max(4, y) + 'px';
  }
  function tipHide() { if (tipEl) tipEl.style.display = 'none'; }

  /** host にホバー処理を1回だけ付ける。描画のたびに host._hover / host._tips を差し替える */
  function hoverable(host) {
    if (host._hb) return;
    host._hb = true;
    host.addEventListener('pointermove', ev => {
      if (host._hover) { tipShow(ev, host._hover(ev)); return; }
      const m = ev.target.closest && ev.target.closest('[data-ti]');
      if (m && host.contains(m) && host._tips) tipShow(ev, host._tips[+m.dataset.ti]);
      else tipHide();
    });
    host.addEventListener('pointerleave', () => { tipHide(); if (host._leave) host._leave(); });
    host.addEventListener('focusin', ev => {
      const m = ev.target.closest('[data-ti]');
      if (m && host._tips) { const rc = m.getBoundingClientRect(); tipShow({ clientX: rc.right, clientY: rc.top }, host._tips[+m.dataset.ti]); }
    });
    host.addEventListener('focusout', tipHide);
  }

  // ===========================================================================
  // 3. SVG チャート部品
  // ===========================================================================
  const width = el => Math.max(300, Math.floor(el.clientWidth || (el.parentNode && el.parentNode.clientWidth) || 720));
  const svgOpen = (W, H) => `<svg class="fbx-svg" width="${W}" height="${H}" viewBox="0 0 ${W} ${H}" role="img">`;
  const lineEl = (x1, y1, x2, y2, cls) => `<line class="${cls}" x1="${r1(x1)}" y1="${r1(y1)}" x2="${r1(x2)}" y2="${r1(y2)}"/>`;

  function scale(d0, d1, a, b) {
    if (d1 === d0) return () => (a + b) / 2;
    const k = (b - a) / (d1 - d0);
    return v => a + (v - d0) * k;
  }

  function niceTicks(lo, hi, n, minStep) {
    if (!isNum(lo) || !isNum(hi)) { lo = 0; hi = 1; }
    if (hi === lo) { const d = Math.abs(lo) * 0.1 || 1; lo -= d; hi += d; }
    const raw = (hi - lo) / (n || 5), mag = Math.pow(10, Math.floor(Math.log10(raw))), e = raw / mag;
    let step = (e >= 7.5 ? 10 : e >= 3.5 ? 5 : e >= 1.5 ? 2 : 1) * mag;
    if (minStep && step < minStep) step = minStep;
    const t0 = Math.floor(lo / step + 1e-9) * step, t1 = Math.ceil(hi / step - 1e-9) * step, out = [];
    for (let v = t0; v <= t1 + step * 1e-6; v += step) out.push(+v.toFixed(10));
    return out;
  }

  /** 軸・目盛・グリッド（実線のヘアライン） */
  function frame(W, H, m, xs, ys, xt, yt, xf, yf, xLab, yLab, vgrid) {
    let g = '';
    yt.forEach(v => {
      const y = r1(ys(v));
      g += `<line class="grid" x1="${m.l}" x2="${W - m.r}" y1="${y}" y2="${y}"/>` +
        `<text class="tick" x="${m.l - 6}" y="${y + 4}" text-anchor="end">${esc(yf(v))}</text>`;
    });
    if (xt && xt.length) {
      const labs = xt.map(v => String(xf(v)));
      const maxw = Math.max(...labs.map(s => textW(s, 11)));
      const spacing = xt.length > 1 ? Math.abs(xs(xt[1]) - xs(xt[0])) : 1e9;
      const step = Math.max(1, Math.ceil((maxw + 8) / Math.max(spacing, 1)));
      xt.forEach((v, i) => {
        const x = r1(xs(v));
        if (vgrid) g += `<line class="grid" x1="${x}" x2="${x}" y1="${m.t}" y2="${H - m.b}"/>`;
        if (i % step === 0) g += `<text class="tick" x="${x}" y="${H - m.b + 16}" text-anchor="middle">${esc(labs[i])}</text>`;
      });
    }
    g += `<line class="axis" x1="${m.l}" x2="${W - m.r}" y1="${H - m.b}" y2="${H - m.b}"/>`;
    if (xLab) g += `<text class="axlab" x="${r1((m.l + W - m.r) / 2)}" y="${H - 6}" text-anchor="middle">${esc(xLab)}</text>`;
    if (yLab) g += `<text class="axlab" transform="translate(12,${r1((m.t + H - m.b) / 2)}) rotate(-90)" text-anchor="middle">${esc(yLab)}</text>`;
    return g;
  }

  /**
   * 折れ線。series: [{nm, color, muted, pts:[{x, y, note, hollow}]}]
   * 色付き系列はクロスヘアのツールチップにまとめて表示、右端に直接ラベル。
   */
  function lineChart(host, cfg) {
    host._tips = null;
    hoverable(host);
    const colored = cfg.series.filter(s => !s.muted);
    const all = [];
    cfg.series.forEach(s => s.pts.forEach(p => { if (isNum(p.y)) all.push(p.y); }));
    if (!all.length) { host._hover = null; host.innerHTML = note('表示できるデータがない。'); return; }

    const W = width(host), H = cfg.H || 320;
    const endW = cfg.endLabels && colored.length ? Math.min(160, 18 + Math.max(...colored.map(s => textW(trunc(s.nm, 16), 11)))) : 18;
    const m = { l: 48, r: endW, t: 14, b: 42 };
    let lo = Math.min(...all), hi = Math.max(...all);
    if (cfg.yZero) { lo = Math.min(lo, 0); hi = Math.max(hi, 0); }
    const yt = cfg.yTicks || niceTicks(lo, hi, 5, cfg.yInt ? 1 : 0);
    const y0 = Math.min(yt[0], lo), y1 = Math.max(yt[yt.length - 1], hi);
    const xs = scale(cfg.x0, cfg.x1, m.l, W - m.r);
    const ys = cfg.yRev ? scale(y0, y1, m.t, H - m.b) : scale(y0, y1, H - m.b, m.t);
    const yf = cfg.yFmt || fmtTick;

    let out = svgOpen(W, H) + frame(W, H, m, xs, ys, cfg.xTicks, yt, cfg.xFmt || String, yf, cfg.xLab, cfg.yLab);
    if (cfg.zeroLine && y0 < 0 && y1 > 0) out += lineEl(m.l, ys(0), W - m.r, ys(0), 'axis');

    const pathOf = pts => {
      let d = '', pen = false;
      pts.forEach(p => {
        if (!isNum(p.y)) { pen = false; return; }
        d += (pen ? 'L' : 'M') + r1(xs(p.x)) + ',' + r1(ys(p.y));
        pen = true;
      });
      return d;
    };
    cfg.series.filter(s => s.muted).forEach(s => {
      out += `<path d="${pathOf(s.pts)}" fill="none" stroke="${MUTED}" stroke-width="1" stroke-linejoin="round" stroke-linecap="round"/>`;
    });
    colored.forEach(s => {
      out += `<path d="${pathOf(s.pts)}" fill="none" stroke="${s.color}" stroke-width="2" stroke-linejoin="round" stroke-linecap="round"/>`;
      if (cfg.markers) s.pts.forEach(p => {
        if (!isNum(p.y)) return;
        out += p.hollow
          ? `<circle cx="${r1(xs(p.x))}" cy="${r1(ys(p.y))}" r="4" fill="${SURFACE}" stroke="${s.color}" stroke-width="2"/>`
          : `<circle cx="${r1(xs(p.x))}" cy="${r1(ys(p.y))}" r="4" fill="${s.color}" stroke="${SURFACE}" stroke-width="2"/>`;
      });
    });

    if (cfg.endLabels && colored.length) {
      const labs = colored.map(s => {
        let last = null;
        s.pts.forEach(p => { if (isNum(p.y)) last = p; });
        return last ? { nm: trunc(s.nm, 16), y: ys(last.y) } : null;
      }).filter(Boolean).sort((a, b) => a.y - b.y);
      for (let i = 1; i < labs.length; i++) if (labs[i].y - labs[i - 1].y < 13) labs[i].y = labs[i - 1].y + 13;
      const over = labs.length ? labs[labs.length - 1].y - (H - m.b) : 0;
      if (over > 0) labs.forEach(l => { l.y -= over; });
      labs.forEach(l => { out += `<text class="lab strong" x="${W - m.r + 6}" y="${r1(l.y + 4)}">${esc(l.nm)}</text>`; });
    }

    out += `<line class="cross" x1="0" x2="0" y1="${m.t}" y2="${H - m.b}" visibility="hidden"/></svg>`;
    host.innerHTML = (cfg.legend && colored.length ? legendHTML(colored.map(s => ({ nm: s.nm, color: s.color }))) : '') + out;

    const svg = host.querySelector('svg'), cross = svg.querySelector('.cross');
    const xVals = cfg.xVals;
    host._leave = () => cross.setAttribute('visibility', 'hidden');
    host._hover = ev => {
      const rc = svg.getBoundingClientRect(), px = ev.clientX - rc.left, py = ev.clientY - rc.top;
      if (!xVals.length || px < m.l - 12 || px > W - m.r + 12 || py < m.t - 12 || py > H - m.b + 12) { host._leave(); return null; }
      let best = xVals[0], bd = Infinity;
      xVals.forEach(v => { const d = Math.abs(xs(v) - px); if (d < bd) { bd = d; best = v; } });
      cross.setAttribute('x1', r1(xs(best))); cross.setAttribute('x2', r1(xs(best)));
      cross.setAttribute('visibility', 'visible');
      const rows = [];
      colored.forEach(s => {
        const p = s.pts.find(q => q.x === best && isNum(q.y));
        if (p) rows.push({ color: s.color, v: (cfg.tipFmt || yf)(p.y), k: s.nm + (p.note ? '（' + p.note + '）' : ''), raw: p.y });
      });
      rows.sort((a, b) => (cfg.yRev ? a.raw - b.raw : b.raw - a.raw));
      const title = cfg.tipTitle ? cfg.tipTitle(best) : String(best);
      return { title, rows: rows.length ? rows : [{ v: '—', k: '色分けした系列にデータなし' }] };
    };
  }

  /** 散布図のラベルを重ならないように置く（優先度順に試し、置けないものは省く） */
  function placeLabels(cands, W, H, m) {
    const boxes = [];
    let out = '';
    cands.forEach(c => {
      const w = textW(c.text, 11), h = 12;
      const tries = [[8, 4, 'start'], [-8, 4, 'end'], [0, -9, 'middle'], [0, 17, 'middle']];
      for (const t of tries) {
        const tx = c.x + t[0], ty = c.y + t[1];
        const bx = t[2] === 'start' ? tx : t[2] === 'end' ? tx - w : tx - w / 2, by = ty - 10;
        if (bx < 2 || bx + w > W - 2 || by < 0 || by + h > H - m.b + 2) continue;
        if (boxes.some(b => bx < b.x + b.w && bx + w > b.x && by < b.y + b.h && by + h > b.y)) continue;
        boxes.push({ x: bx, y: by, w, h });
        out += `<text class="lab${c.strong ? ' strong' : ''}" x="${r1(tx)}" y="${r1(ty)}" text-anchor="${t[2]}">${esc(c.text)}</text>`;
        break;
      }
    });
    return out;
  }

  /**
   * 散布図。pts: [{x, y, color, muted, label, labelPri, strong, tip}]
   * 最寄りの点（24px 以内）にツールチップ。diag / medians / reg は参照線。
   */
  function scatterChart(host, cfg) {
    host._tips = null;
    hoverable(host);
    const pts = cfg.pts.filter(p => isNum(p.x) && isNum(p.y));
    if (!pts.length) { host._hover = null; host.innerHTML = note('表示できるデータがない。'); return null; }

    const W = width(host), H = cfg.H || 400, m = { l: 54, r: 18, t: 14, b: 44 };
    let xl = [Math.min(...pts.map(p => p.x)), Math.max(...pts.map(p => p.x))];
    let yl = [Math.min(...pts.map(p => p.y)), Math.max(...pts.map(p => p.y))];
    if (cfg.diag) { const lo = Math.min(xl[0], yl[0]), hi = Math.max(xl[1], yl[1]); xl = [lo, hi]; yl = [lo, hi]; }
    const px = (xl[1] - xl[0]) * 0.05 || 0.5, py = (yl[1] - yl[0]) * 0.05 || 0.5;
    // 値がすべて0以上なら、余白のせいで軸がマイナスに伸びないようにする
    const lo = (v, pad) => (v >= 0 && v - pad < 0 ? 0 : v - pad);
    const xt = niceTicks(lo(xl[0], px), xl[1] + px, 6), yt = niceTicks(lo(yl[0], py), yl[1] + py, 5);
    const X0 = xt[0], X1 = xt[xt.length - 1], Y0 = yt[0], Y1 = yt[yt.length - 1];
    const xs = scale(X0, X1, m.l, W - m.r), ys = scale(Y0, Y1, H - m.b, m.t);
    const clipId = 'c' + Math.random().toString(36).slice(2, 9);

    let out = svgOpen(W, H) +
      `<defs><clipPath id="${clipId}"><rect x="${m.l}" y="${m.t}" width="${W - m.l - m.r}" height="${H - m.t - m.b}"/></clipPath></defs>` +
      frame(W, H, m, xs, ys, xt, yt, cfg.xFmt || fmtTick, cfg.yFmt || fmtTick, cfg.xLab, cfg.yLab, true);

    let stats = null;
    out += `<g clip-path="url(#${clipId})">`;
    if (cfg.diag) {
      const a = Math.max(X0, Y0), b = Math.min(X1, Y1);
      out += lineEl(xs(a), ys(a), xs(b), ys(b), 'ref');
    }
    if (cfg.medians) {
      const mx = median(pts.map(p => p.x)), my = median(pts.map(p => p.y));
      out += lineEl(xs(mx), m.t, xs(mx), H - m.b, 'ref') + lineEl(m.l, ys(my), W - m.r, ys(my), 'ref');
    }
    if (cfg.reg && pts.length >= 3) {
      const n = pts.length, sx = pts.reduce((s, p) => s + p.x, 0) / n, sy = pts.reduce((s, p) => s + p.y, 0) / n;
      let sxy = 0, sxx = 0, syy = 0;
      pts.forEach(p => { sxy += (p.x - sx) * (p.y - sy); sxx += (p.x - sx) ** 2; syy += (p.y - sy) ** 2; });
      if (sxx > 0) {
        const b = sxy / sxx, a = sy - b * sx;
        stats = { r: syy > 0 ? sxy / Math.sqrt(sxx * syy) : null, slope: b, n };
        out += lineEl(xs(X0), ys(a + b * X0), xs(X1), ys(a + b * X1), 'ref');
      }
    }
    out += '</g>';
    if (cfg.diag) out += `<text class="reflab" x="${W - m.r - 4}" y="${m.t + 12}" text-anchor="end">対角線 = 同じ値</text>`;
    if (cfg.medians) out += `<text class="reflab" x="${W - m.r - 4}" y="${H - m.b - 6}" text-anchor="end">線 = 中央値</text>`;
    if (cfg.quadrants) {
      const q = cfg.quadrants;
      if (q.tl) out += `<text class="reflab" x="${m.l + 6}" y="${m.t + 12}">${esc(q.tl)}</text>`;
      if (q.br) out += `<text class="reflab" x="${W - m.r - 6}" y="${H - m.b - 20}" text-anchor="end">${esc(q.br)}</text>`;
    }

    const order = pts.slice().sort((a, b) => (a.muted ? 0 : 1) - (b.muted ? 0 : 1));
    order.forEach(p => {
      out += `<circle cx="${r1(xs(p.x))}" cy="${r1(ys(p.y))}" r="${p.r || 5}" fill="${p.muted ? MUTED : p.color}" stroke="${SURFACE}" stroke-width="2"/>`;
    });
    const cands = pts.filter(p => p.label && p.labelPri > 0)
      .sort((a, b) => b.labelPri - a.labelPri)
      .map(p => ({ x: xs(p.x), y: ys(p.y), text: trunc(p.label, 20), strong: p.strong }));
    out += placeLabels(cands, W, H, m);
    out += '<circle class="hover-ring" r="9" visibility="hidden"/></svg>';
    host.innerHTML = out;

    const svg = host.querySelector('svg'), ring = svg.querySelector('.hover-ring');
    host._leave = () => ring.setAttribute('visibility', 'hidden');
    host._hover = ev => {
      const rc = svg.getBoundingClientRect(), mx = ev.clientX - rc.left, my = ev.clientY - rc.top;
      let best = null, bd = 24 * 24;
      pts.forEach(p => { const d = (xs(p.x) - mx) ** 2 + (ys(p.y) - my) ** 2; if (d < bd) { bd = d; best = p; } });
      if (!best) { host._leave(); return null; }
      ring.setAttribute('cx', r1(xs(best.x))); ring.setAttribute('cy', r1(ys(best.y)));
      ring.setAttribute('visibility', 'visible');
      return best.tip;
    };
    return stats;
  }

  /** 棒の上端だけ角丸（ベースライン側は角ばらせる） */
  function barPath(x, y, w, h, r) {
    r = Math.max(0, Math.min(r, w / 2, h));
    return `M${r1(x)},${r1(y + h)}V${r1(y + r)}Q${r1(x)},${r1(y)} ${r1(x + r)},${r1(y)}H${r1(x + w - r)}Q${r1(x + w)},${r1(y)} ${r1(x + w)},${r1(y + r)}V${r1(y + h)}Z`;
  }

  /** 縦棒（グループ）。series: [{nm, color, v:[]}]。カテゴリ単位でツールチップ */
  function barChart(host, cfg) {
    host._hover = null; host._leave = null;
    hoverable(host);
    const W = width(host), H = cfg.H || 280, m = { l: 42, r: 12, t: 18, b: 42 };
    const n = cfg.cats.length, k = cfg.series.length;
    let mx = 1;
    cfg.series.forEach(s => s.v.forEach(v => { if (v > mx) mx = v; }));
    const yt = niceTicks(0, mx, 5, 1), ys = scale(0, yt[yt.length - 1], H - m.b, m.t);
    const band = (W - m.l - m.r) / Math.max(n, 1), inner = band * 0.74;
    const bw = Math.max(2, (inner - 2 * (k - 1)) / Math.max(k, 1));
    const xc = i => m.l + band * (i + 0.5);
    let out = svgOpen(W, H) + frame(W, H, m, xc, ys, cfg.cats.map((_, i) => i), yt, i => cfg.cats[i], fmtTick, cfg.xLab, cfg.yLab);
    const tips = [];
    cfg.cats.forEach((c, i) => {
      const gx = m.l + band * i + (band - inner) / 2;
      cfg.series.forEach((s, j) => {
        const v = s.v[i] || 0, x = gx + j * (bw + 2), y = ys(v), h = (H - m.b) - y;
        if (h > 0) out += `<path d="${barPath(x, y, bw, h, 4)}" fill="${s.color}"/>`;
        if (bw >= 14 && v > 0) out += `<text class="tick" x="${r1(x + bw / 2)}" y="${r1(y - 4)}" text-anchor="middle">${v}</text>`;
      });
      tips.push({ title: cfg.tipTitle ? cfg.tipTitle(i) : c, rows: cfg.series.map(s => ({ color: s.color, v: fx.int(s.v[i] || 0), k: s.nm })) });
    });
    cfg.cats.forEach((_, i) => {
      out += `<rect data-ti="${i}" x="${r1(m.l + band * i)}" y="${m.t}" width="${r1(band)}" height="${H - m.t - m.b}" fill="transparent"/>`;
    });
    out += '</svg>';
    host.innerHTML = (cfg.legend ? legendHTML(cfg.series.map(s => ({ nm: s.nm, color: s.color }))) : '') + out;
    host._tips = tips;
  }

  /**
   * ハーフピッチのシュートマップ（ゴールが上）。
   * Understat 座標：X = 自陣ゴールラインからの距離の割合、Y = 横方向の割合。
   */
  function pitchChart(host, list, colorOf, tipOf) {
    host._tips = null;
    hoverable(host);
    const W = Math.min(width(host), 560), pad = 16, k = (W - 2 * pad) / 68, L = 52.5;
    const H = Math.round(L * k + pad * 2);
    const X = y => pad + y * k, Y = d => pad + d * k;
    let out = svgOpen(W, H);
    out += `<rect class="pitch" x="${X(0)}" y="${Y(0)}" width="${r1(68 * k)}" height="${r1(L * k)}"/>`;
    out += `<rect class="pitch" x="${r1(X(34 - 20.16))}" y="${Y(0)}" width="${r1(40.32 * k)}" height="${r1(16.5 * k)}"/>`;
    out += `<rect class="pitch" x="${r1(X(34 - 9.16))}" y="${Y(0)}" width="${r1(18.32 * k)}" height="${r1(5.5 * k)}"/>`;
    out += `<rect class="pitch" x="${r1(X(34 - 3.66))}" y="${r1(Y(0) - 6)}" width="${r1(7.32 * k)}" height="6"/>`;
    out += `<circle cx="${r1(X(34))}" cy="${r1(Y(11))}" r="1.8" fill="${MUTED}"/>`;
    const hw = Math.sqrt(9.15 * 9.15 - 5.5 * 5.5);
    out += `<path class="pitch" d="M${r1(X(34 - hw))},${r1(Y(16.5))} A${r1(9.15 * k)},${r1(9.15 * k)} 0 0 0 ${r1(X(34 + hw))},${r1(Y(16.5))}"/>`;
    out += `<path class="pitch" d="M${r1(X(34 - 9.15))},${r1(Y(L))} A${r1(9.15 * k)},${r1(9.15 * k)} 0 0 1 ${r1(X(34 + 9.15))},${r1(Y(L))}"/>`;

    const pos = s => ({ cx: X((1 - s.y) * 68), cy: Y(Math.min(L, Math.max(0, (1 - s.x) * 105))), r: 3 + Math.sqrt(Math.max(0, s.xg)) * 12 });
    const P = list.map(s => Object.assign({ s }, pos(s)));
    P.filter(p => !p.s.goal).forEach(p => {
      out += `<circle cx="${r1(p.cx)}" cy="${r1(p.cy)}" r="${r1(p.r)}" fill="${SURFACE}" fill-opacity="0.6" stroke="${colorOf(p.s)}" stroke-width="1.5"/>`;
    });
    P.filter(p => p.s.goal).forEach(p => {
      out += `<circle cx="${r1(p.cx)}" cy="${r1(p.cy)}" r="${r1(p.r)}" fill="${colorOf(p.s)}" stroke="${SURFACE}" stroke-width="1.5"/>`;
    });
    out += '<circle class="hover-ring" r="10" visibility="hidden"/></svg>';
    host.innerHTML = out;

    const svg = host.querySelector('svg'), ring = svg.querySelector('.hover-ring');
    host._leave = () => ring.setAttribute('visibility', 'hidden');
    host._hover = ev => {
      const rc = svg.getBoundingClientRect(), mx = ev.clientX - rc.left, my = ev.clientY - rc.top;
      let best = null, bd = 24 * 24;
      P.forEach(p => { const d = (p.cx - mx) ** 2 + (p.cy - my) ** 2; if (d < bd) { bd = d; best = p; } });
      if (!best) { host._leave(); return null; }
      ring.setAttribute('cx', r1(best.cx)); ring.setAttribute('cy', r1(best.cy));
      ring.setAttribute('r', r1(best.r + 3)); ring.setAttribute('visibility', 'visible');
      return tipOf(best.s);
    };
  }

  // ===========================================================================
  // 4. 並び替えできる表
  // ===========================================================================
  /** cols: [{k, lab, num, v:(row)=>値, f:(row)=>表示文字列, html:(row)=>HTML}] */
  function tableHTML(cols, rows, ts, opt = {}) {
    const col = cols.find(c => c.k === ts.key) || cols[0];
    const sorted = rows.slice();
    const bad = v => v == null || (typeof v === 'number' && !isFinite(v));
    sorted.sort((a, b) => {
      const va = col.v(a), vb = col.v(b);
      if (bad(va) && bad(vb)) return 0;
      if (bad(va)) return 1;
      if (bad(vb)) return -1;
      return (typeof va === 'string' ? va.localeCompare(vb, 'ja') : va - vb) * ts.dir;
    });
    const view = sorted.slice(0, ts.limit || 1e9);
    const head = cols.map(c =>
      `<th class="${c.num ? 'num ' : ''}${c === col ? 'sorted' : ''}" data-k="${esc(c.k)}" tabindex="0">${esc(c.lab)}${c === col ? (ts.dir > 0 ? ' ▲' : ' ▼') : ''}</th>`).join('');
    const body = view.map(r =>
      `<tr${opt.hl && opt.hl(r) ? ' class="hl"' : ''}>` +
      cols.map(c => `<td${c.num ? ' class="num"' : ''}>${c.html ? c.html(r) : esc(c.f ? c.f(r) : c.v(r))}</td>`).join('') +
      '</tr>').join('');
    return `<div class="fbx-scroll"><table class="fbx-t"><thead><tr>${head}</tr></thead><tbody>${body}</tbody></table></div>` +
      `<div class="fbx-note">${sorted.length} 件中 ${view.length} 件を表示。見出しをクリックで並び替え。` +
      (sorted.length > view.length ? ` <button class="fbx-more">さらに表示</button>` : '') + (opt.foot ? ' ' + opt.foot : '') + '</div>';
  }
  function wireTable(host, ts, redraw, step = 50) {
    const sortBy = th => {
      const k = th.dataset.k;
      if (ts.key === k) ts.dir = -ts.dir;
      else { ts.key = k; ts.dir = th.classList.contains('num') ? -1 : 1; }
      redraw();
    };
    host.addEventListener('click', e => {
      const th = e.target.closest('th[data-k]');
      if (th && host.contains(th)) { sortBy(th); return; }
      if (e.target.closest('.fbx-more')) { ts.limit += step; redraw(); }
    });
    host.addEventListener('keydown', e => {
      const th = e.target.closest('th[data-k]');
      if (th && (e.key === 'Enter' || e.key === ' ')) { e.preventDefault(); sortBy(th); }
    });
  }
  const nCol = (k, labText, v, f) => ({
    k, lab: labText, num: true, v,
    f: r => { const x = v(r); return x == null || (typeof x === 'number' && !isFinite(x)) ? '—' : (f || fx.int)(x); }
  });
  const tCol = (k, labText, v) => ({ k, lab: labText, v });

  // ===========================================================================
  // 5. 共有状態：シーズンと節の範囲
  // ===========================================================================
  const st = { s: Math.min(DEFAULT_S, Math.max(0, SEASONS.length - 1)), preset: 'all', from: 1, to: 38 };
  const PRESETS = [['all', '全節'], ['first', '前半戦'], ['second', '後半戦'], ['last10', '直近10節'], ['last5', '直近5節']];

  /** 現在の範囲。節 = 各チームの第n試合 */
  function range() {
    const mx = Math.max(1, maxMd(st.s)), half = Math.max(1, teamCount(st.s) - 1);
    let f = 1, t = mx;
    switch (st.preset) {
      case 'first': t = Math.min(half, mx); break;
      case 'second': f = Math.min(half + 1, mx); break;
      case 'last10': f = Math.max(1, mx - 9); break;
      case 'last5': f = Math.max(1, mx - 4); break;
      case 'custom': t = Math.min(Math.max(1, st.to), mx); f = Math.min(Math.max(1, st.from), t); break;
      default: break;
    }
    return { s: st.s, from: f, to: t, max: mx, full: f === 1 && t === mx };
  }
  const rangeText = rg => rg.full ? `${SLAB[rg.s]} 全${rg.max}節` : `${SLAB[rg.s]} 第${rg.from}〜${rg.to}節`;

  const ctlHosts = [], subs = [];
  function notify() {
    ctlHosts.forEach(paintCtl);
    subs.forEach(f => f());
  }
  function paintCtl(c) {
    const rg = range();
    let html = row('シーズン', SEASONS.map((_, i) =>
      `<button class="fbx-season${i === st.s ? ' on' : ''}" data-s="${i}">${esc(SLAB[i])}</button>`).join(''));
    if (c.md) {
      const opts = sel => Array.from({ length: rg.max }, (_, i) =>
        `<option value="${i + 1}"${i + 1 === sel ? ' selected' : ''}>第${i + 1}節</option>`).join('');
      html += row('節', PRESETS.map(p =>
        `<button class="fbx-preset${st.preset === p[0] ? ' on' : ''}" data-p="${p[0]}">${p[1]}</button>`).join('') +
        `<select class="fbx-from">${opts(rg.from)}</select><span class="fbx-sep">〜</span><select class="fbx-to">${opts(rg.to)}</select>` +
        `<span class="fbx-note2">${rg.to - rg.from + 1}節分（各チームの第n試合で数える）</span>`);
    } else {
      html += row('', `<span class="fbx-note2">${c.note || 'この章はシーズン全体の数字（節の範囲指定は反映しない）'}</span>`);
    }
    c.el.innerHTML = html + '<div class="fbx-divider"></div>';
  }
  function control(el, opt = {}) {
    const c = { el, md: !!opt.md, note: opt.note };
    ctlHosts.push(c);
    el.addEventListener('click', e => {
      const b = e.target.closest('button');
      if (!b) return;
      if (b.dataset.s != null) { st.s = +b.dataset.s; notify(); }
      else if (b.dataset.p) { st.preset = b.dataset.p; notify(); }
    });
    el.addEventListener('change', e => {
      const t = e.target, rg = range(), v = +t.value;
      if (t.classList.contains('fbx-from')) { st.preset = 'custom'; st.from = v; st.to = Math.max(rg.to, v); notify(); }
      if (t.classList.contains('fbx-to')) { st.preset = 'custom'; st.to = v; st.from = Math.min(rg.from, v); notify(); }
    });
    paintCtl(c);
  }

  function shell(id, parts) {
    const root = document.getElementById(id);
    root.className = 'fbx';
    root.innerHTML = parts.map(p => `<div data-part="${p}"></div>`).join('');
    const P = { root };
    parts.forEach(p => { P[p] = root.querySelector(`[data-part="${p}"]`); });
    return P;
  }

  /** 描画関数を登録：状態変更と幅の変化で再描画 */
  function reg(root, draw) {
    subs.push(draw);
    let lastW = 0, queued = false;
    if (window.ResizeObserver) {
      new ResizeObserver(ent => {
        const w = Math.round(ent[0].contentRect.width);
        if (lastW && Math.abs(w - lastW) > 4 && !queued) {
          queued = true;
          requestAnimationFrame(() => { queued = false; draw(); });
        }
        lastW = w;
      }).observe(root);
    }
    draw();
  }

  /** チップ（色分けの凡例を兼ねる）。items: [{id, nm, sub}] */
  function chipsHTML(labText, items, pool, presets, flash, removable) {
    const pre = (presets || []).map(p => `<button data-preset="${p[0]}">${p[1]}</button>`).join('');
    return row(labText, pre + `<span class="fbx-note2">色分け ${pool.size} / 8</span>` + (flash ? `<span class="fbx-flash">${esc(flash)}</span>` : '')) +
      row('', items.map(it => {
        const c = pool.color(it.id);
        return `<button class="fbx-chip${c ? ' on' : ''}" data-id="${it.id}">${c ? `<i style="background:${c}"></i>` : ''}${esc(it.nm)}` +
          (it.sub ? `<small>${esc(it.sub)}</small>` : '') + (removable ? '<span class="x">×</span>' : '') + '</button>';
      }).join(''));
  }
  function wireChips(host, pool, redraw, onPreset) {
    host.addEventListener('click', e => {
      const b = e.target.closest('button');
      if (!b || !host.contains(b)) return;
      if (b.dataset.id != null) {
        const id = +b.dataset.id;
        if (pool.has(id)) pool.remove(id);
        else if (!pool.add(id)) host._flash = '色分けは8件まで。どれかを外してから追加する。';
        redraw();
      } else if (b.dataset.preset && onPreset) { onPreset(b.dataset.preset); redraw(); }
    });
  }
  const takeFlash = host => { const f = host._flash; host._flash = ''; return f; };

  /** 候補リストから選んだ（または名前を正確に入力して確定した）ときに onPick を呼ぶ */
  function wirePicker(host, cls, onPick) {
    const take = t => {
      const i = PLAYERS.indexOf(t.value.trim());
      if (i < 0) return;
      t.value = '';
      onPick(i);
    };
    host.addEventListener('input', e => {
      if (e.target.classList.contains(cls) && (e.inputType === 'insertReplacementText' || e.inputType == null)) take(e.target);
    });
    host.addEventListener('change', e => { if (e.target.classList.contains(cls)) take(e.target); });
    host.addEventListener('keydown', e => { if (e.target.classList.contains(cls) && e.key === 'Enter') take(e.target); });
  }
  const pickerHTML = (cls, listId, names, ph) =>
    `<input type="search" class="${cls}" list="${listId}" placeholder="${esc(ph)}"><datalist id="${listId}">` +
    names.map(n => `<option value="${esc(n)}"></option>`).join('') + '</datalist>';

  // ===========================================================================
  // 6. 集計
  // ===========================================================================
  /** 範囲・会場を指定した順位表。同勝点は得失点差→総得点 */
  function standings(s, from, to, venue = 'all') {
    return memo(`st|${s}|${from}|${to}|${venue}`, () => {
      const acc = new Map();
      seasonRace(s).forEach(r => {
        if (r.n < from || r.n > to) return;
        if ((venue === 'H' && !r.h) || (venue === 'A' && r.h)) return;
        let o = acc.get(r.t);
        if (!o) { o = { t: r.t, MP: 0, W: 0, D: 0, L: 0, Pts: 0, G: 0, GA: 0, xG: 0, xGA: 0, xPts: 0 }; acc.set(r.t, o); }
        o.MP++; o[r.res]++; o.Pts += r.pt; o.G += r.g; o.GA += r.ga; o.xG += r.xg; o.xGA += r.xga; o.xPts += r.xp;
      });
      const rows = [...acc.values()];
      rows.forEach(o => {
        o.GD = o.G - o.GA; o.xGD = o.xG - o.xGA;
        o.GmxG = o.G - o.xG; o.GAmxGA = o.GA - o.xGA; o.PmxP = o.Pts - o.xPts;
      });
      rows.sort((a, b) => b.Pts - a.Pts || b.GD - a.GD || b.G - a.G || TEAMS[a.t].localeCompare(TEAMS[b.t]));
      rows.forEach((o, i) => { o.rank = i + 1; });
      rows.slice().sort((a, b) => b.xPts - a.xPts).forEach((o, i) => { o.xRank = i + 1; });
      return rows;
    });
  }
  const fullStandings = s => standings(s, 1, maxMd(s), 'all');

  const TM = [
    { k: 'Pts', lab: '勝点', f: fx.int }, { k: 'xPts', lab: '期待勝点 xPts', f: fx.d1 }, { k: 'PmxP', lab: '勝点−xPts', f: fx.s1 },
    { k: 'G', lab: '得点', f: fx.int }, { k: 'xG', lab: 'xG', f: fx.d1 }, { k: 'GmxG', lab: '得点−xG', f: fx.s1 },
    { k: 'GA', lab: '失点', f: fx.int }, { k: 'xGA', lab: '被xG（xGA）', f: fx.d1 }, { k: 'GAmxGA', lab: '失点−xGA', f: fx.s1 },
    { k: 'GD', lab: '得失点差', f: fx.s0 }, { k: 'xGD', lab: 'xG差（xGD）', f: fx.s1 }, { k: 'rank', lab: '順位', f: fx.int }
  ];
  const tmDef = k => TM.find(x => x.k === k) || TM[0];
  const tmVal = (o, k, per) => (per && k !== 'rank' ? div(o[k], o.MP) : o[k]);
  const tmFmt = (k, per) => (per && k !== 'rank' ? fx.d2 : tmDef(k).f);
  const tmLab = (k, per) => tmDef(k).lab + (per && k !== 'rank' ? '／試合' : '');

  /** 範囲内のシュートから選手×チームの成績を作る（アシスト側から KP・xA も） */
  function playerStats(s, from, to) {
    return memo(`ps|${s}|${from}|${to}`, () => {
      const acc = new Map();
      const get = (p, t) => {
        const key = p + '|' + t;
        let o = acc.get(key);
        if (!o) { o = { p, t, sh: 0, g: 0, xg: 0, npsh: 0, npg: 0, npxg: 0, kp: 0, xa: 0, a: 0 }; acc.set(key, o); }
        return o;
      };
      seasonShots(s).forEach(x => {
        if (!(x.n >= from && x.n <= to) || x.p < 0) return;
        const o = get(x.p, x.t);
        o.sh++; o.xg += x.xg;
        if (x.goal) o.g++;
        if (!x.pen) { o.npsh++; o.npxg += x.xg; if (x.goal) o.npg++; }
        if (x.a >= 0) { const q = get(x.a, x.t); q.kp++; q.xa += x.xg; if (x.goal) q.a++; }
      });
      const rows = [...acc.values()];
      rows.forEach(o => {
        o.gmx = o.g - o.xg; o.ga = o.g + o.a; o.xgxa = o.xg + o.xa;
        o.conv = div(o.g, o.sh); o.xgps = div(o.xg, o.sh);
      });
      return rows;
    });
  }
  const PM = [
    { k: 'g', lab: '得点', f: fx.int }, { k: 'xg', lab: 'xG', f: fx.d2 }, { k: 'gmx', lab: '得点−xG', f: fx.s2 },
    { k: 'sh', lab: 'シュート', f: fx.int }, { k: 'npg', lab: '非PK得点', f: fx.int }, { k: 'npxg', lab: 'npxG', f: fx.d2 },
    { k: 'a', lab: 'アシスト', f: fx.int }, { k: 'xa', lab: 'xA', f: fx.d2 }, { k: 'kp', lab: 'キーパス', f: fx.int },
    { k: 'ga', lab: '得点+アシスト', f: fx.int }, { k: 'xgxa', lab: 'xG+xA', f: fx.d2 },
    { k: 'conv', lab: '決定率', f: fx.pct }, { k: 'xgps', lab: 'xG／シュート', f: fx.d3 }
  ];
  const pmDef = k => PM.find(x => x.k === k) || PM[0];

  // ---- オフサイド：収録試合数の確認 ----
  offT.forEach(r => {
    r.kaPm = div(r.ka, r.gm); r.kePm = div(r.ke, r.gm);
    r.diffPm = isNum(r.kePm) && isNum(r.kaPm) ? r.kePm - r.kaPm : null;
    r.trap = div(r.ke, (r.ke || 0) + (r.ka || 0));
  });
  const SOURCES = A(D.offSources);            // 例: ['FBref', 'ESPN']
  const offMedian = {}, offFull = {}, offSrc = {};
  {
    const g = {};
    offT.forEach(r => {
      (g[r.lg + '|' + r.y] = g[r.lg + '|' + r.y] || []).push(r.gm);
      offSrc[r.lg + '|' + r.y] = SOURCES[r.src] || '';
    });
    // 揃っているシーズンの試合数 = 2 ×（チーム数 − 1）。18チーム制なら34、20チーム制なら38
    Object.keys(g).forEach(key => {
      offMedian[key] = median(g[key]);
      offFull[key] = 2 * (g[key].length - 1);
    });
  }
  // リーグ内順位（1試合あたり、多い順）
  {
    const g = {};
    offT.forEach(r => { (g[r.lg + '|' + r.y] = g[r.lg + '|' + r.y] || []).push(r); });
    Object.values(g).forEach(arr => {
      arr.slice().sort((a, b) => (b.kePm || 0) - (a.kePm || 0)).forEach((r, i) => { r.keRank = i + 1; });
      arr.slice().sort((a, b) => (b.kaPm || 0) - (a.kaPm || 0)).forEach((r, i) => { r.kaRank = i + 1; });
      arr.forEach(r => { r.nTeams = arr.length; });
    });
  }
  /** 収録率（そのシーズンのチーム数から計算した試合数に対する割合）。データなしは null */
  const offCoverage = (lg, y) => (offMedian[lg + '|' + y] == null ? null : offMedian[lg + '|' + y] / (offFull[lg + '|' + y] || 1));
  const offPartial = (lg, y) => { const c = offCoverage(lg, y); return c != null && c < 0.9; };
  const offSourceNote = (lg, y) => {
    const src = offSrc[lg + '|' + y], md = Math.round(offMedian[lg + '|' + y] || 0);
    if (!offPartial(lg, y)) return src;
    return src === 'ESPN' ? `${src}・シーズン途中（約${md}試合）` : `${src}・データ途中まで（約${md}試合）`;
  };
  function coverageNotice(lgs, y) {
    const stale = [], live = [];
    lgs.forEach(lg => {
      const c = offCoverage(lg, y), md = Math.round(offMedian[lg + '|' + y] || 0);
      if (c == null) stale.push(`${LEAGUE_LAB[lg]}：${seasonLabelOf(y)} のデータなし`);
      else if (c < 0.9) (offSrc[lg + '|' + y] === 'ESPN' ? live : stale).push(`${LEAGUE_LAB[lg]}：1チーム約 ${md} 試合`);
    });
    let html = '';
    if (live.length) html += notice(`<b>シーズン途中</b>：${esc(seasonLabelOf(y))} は進行中のため、終わった試合までの集計。` + live.map(esc).join(' ／ '));
    if (stale.length) html += notice('<b>注意</b>：オフサイドの数字が不完全なシーズンがある。' + stale.map(esc).join(' ／ '));
    return html;
  }
  /** Understat のチーム → FBref のチーム名（シーズンごと） */
  const fbSquadOf = (s, t) => { const m = tmap.find(r => r.s === s && r.t === t); return m && m.sq >= 0 ? m.sq : -1; };

  // ===========================================================================
  // 6b. 統計（相関・信頼区間・p値）
  // ===========================================================================
  function lgamma(x) {
    const c = [0.99999999999980993, 676.5203681218851, -1259.1392167224028, 771.32342877765313, -176.61502916214059,
      12.507343278686905, -0.13857109526572012, 9.9843695780195716e-6, 1.5056327351493116e-7];
    if (x < 0.5) return Math.log(Math.PI / Math.abs(Math.sin(Math.PI * x))) - lgamma(1 - x);
    x -= 1;
    let a = c[0];
    const t = x + 7.5;
    for (let i = 1; i < 9; i++) a += c[i] / (x + i);
    return 0.5 * Math.log(2 * Math.PI) + (x + 0.5) * Math.log(t) - t + Math.log(a);
  }
  function betacf(a, b, x) {
    const FPMIN = 1e-300, qab = a + b, qap = a + 1, qam = a - 1;
    let c = 1, d = 1 - qab * x / qap;
    if (Math.abs(d) < FPMIN) d = FPMIN;
    d = 1 / d;
    let h = d;
    for (let m = 1; m <= 300; m++) {
      const m2 = 2 * m;
      let aa = m * (b - m) * x / ((qam + m2) * (a + m2));
      d = 1 + aa * d; if (Math.abs(d) < FPMIN) d = FPMIN;
      c = 1 + aa / c; if (Math.abs(c) < FPMIN) c = FPMIN;
      d = 1 / d; h *= d * c;
      aa = -(a + m) * (qab + m) * x / ((a + m2) * (qap + m2));
      d = 1 + aa * d; if (Math.abs(d) < FPMIN) d = FPMIN;
      c = 1 + aa / c; if (Math.abs(c) < FPMIN) c = FPMIN;
      d = 1 / d;
      const del = d * c;
      h *= del;
      if (Math.abs(del - 1) < 3e-14) break;
    }
    return h;
  }
  /** 正則化不完全ベータ関数 I_x(a, b) */
  function ibeta(x, a, b) {
    if (x <= 0) return 0;
    if (x >= 1) return 1;
    const bt = Math.exp(lgamma(a + b) - lgamma(a) - lgamma(b) + a * Math.log(x) + b * Math.log(1 - x));
    return x < (a + 1) / (a + b + 2) ? bt * betacf(a, b, x) / a : 1 - bt * betacf(b, a, 1 - x) / b;
  }
  /** t 検定の両側 p 値 */
  const tPvalue = (t, df) => (isFinite(t) ? ibeta(df / (df + t * t), df / 2, 0.5) : 0);
  function ranks(v) {
    const idx = v.map((x, i) => [x, i]).sort((a, b) => a[0] - b[0]), out = new Array(v.length);
    for (let i = 0; i < idx.length;) {
      let j = i;
      while (j + 1 < idx.length && idx[j + 1][0] === idx[i][0]) j++;
      for (let k = i; k <= j; k++) out[idx[k][1]] = (i + j) / 2 + 1;   // 同順位は平均順位
      i = j + 1;
    }
    return out;
  }
  function pearson(x, y) {
    const n = x.length, mx = x.reduce((s, v) => s + v, 0) / n, my = y.reduce((s, v) => s + v, 0) / n;
    let sxy = 0, sxx = 0, syy = 0;
    for (let i = 0; i < n; i++) { sxy += (x[i] - mx) * (y[i] - my); sxx += (x[i] - mx) ** 2; syy += (y[i] - my) ** 2; }
    return { r: sxx > 0 && syy > 0 ? sxy / Math.sqrt(sxx * syy) : null, slope: sxx > 0 ? sxy / sxx : null, mx, my };
  }
  /** ピアソン r（95%CI は Fisher の z 変換）・p 値・スピアマン ρ・回帰の傾き */
  function corStats(x, y) {
    const n = x.length;
    if (n < 5) return null;
    const pr = pearson(x, y);
    if (pr.r == null) return null;
    const r = Math.max(-0.999999, Math.min(0.999999, pr.r));
    const se = 1 / Math.sqrt(n - 3), z = Math.atanh(r);
    const rho = pearson(ranks(x), ranks(y)).r;
    const tOf = v => v * Math.sqrt((n - 2) / (1 - v * v));
    return {
      n, r, lo: Math.tanh(z - 1.96 * se), hi: Math.tanh(z + 1.96 * se), p: tPvalue(tOf(r), n - 2),
      rho, pRho: rho == null ? null : tPvalue(tOf(Math.max(-0.999999, Math.min(0.999999, rho))), n - 2),
      slope: pr.slope, intercept: pr.my - pr.slope * pr.mx
    };
  }
  /** R から来た帰無シミュレーションの結果（スカラーは [値] で届くので展開する） */
  const NULL_SIM = Object.fromEntries(Object.entries((D.corr && D.corr.null) || {})
    .map(([k, v]) => [k, k === 'centers' || k === 'counts' ? A(v) : one(v)]));
  const fmtP = p => (!isNum(p) ? '—' : p < 0.001 ? 'p < 0.001' : 'p = ' + p.toFixed(3));
  const strength = r => (Math.abs(r) < 0.1 ? 'ほぼ無い' : Math.abs(r) < 0.3 ? '弱い' : Math.abs(r) < 0.5 ? '中程度の' : '強い');
  function judgeText(s) {
    if (!s) return 'データが少なすぎて判定できない。';
    if (s.p >= 0.05) return `統計的に有意な相関は見られない（r = ${s.r.toFixed(3)}、${fmtP(s.p)}）。`;
    return `${strength(s.r)}${s.r < 0 ? '負' : '正'}の相関があり、統計的に有意（r = ${s.r.toFixed(3)}、95%CI ${s.lo.toFixed(3)}〜${s.hi.toFixed(3)}、${fmtP(s.p)}）。` +
      (s.r < 0 ? 'かけた数が多いチームほど、かかった数が少ない傾向。' : 'かけた数が多いチームほど、かかった数も多い傾向。');
  }

  /**
   * 信頼区間つきの点（フォレストプロット）。items: [{nm, est, lo, hi, sub, tip}]
   * band: {lo, hi, label} を灰色の帯で重ねる（帰無分布の範囲など）
   */
  function ciChart(host, items, cfg = {}) {
    host._hover = null; host._leave = null;
    hoverable(host);
    const list = items.filter(it => isNum(it.est));
    if (!list.length) { host.innerHTML = note('表示できるデータがない。'); host._tips = null; return; }
    const W = width(host), rowH = 26, labW = Math.min(170, Math.max(...list.map(it => textW(it.nm, 11.5))) + 14);
    const valW = 150, m = { l: labW, r: valW, t: 10, b: 38 };
    const H = m.t + m.b + rowH * list.length;
    const vals = list.flatMap(it => [it.lo, it.hi, it.est]).filter(isNum).concat([0]);
    if (cfg.band) vals.push(cfg.band.lo, cfg.band.hi);
    // 目盛りの数は描画幅に合わせる（狭い列で数字が重ならないように）
    const xt = niceTicks(Math.min(...vals), Math.max(...vals), Math.max(2, Math.min(6, Math.floor((W - m.l - m.r) / 55))));
    const xs = scale(xt[0], xt[xt.length - 1], m.l, W - m.r);
    let out = svgOpen(W, H);
    xt.forEach(v => {
      out += `<line class="grid" x1="${r1(xs(v))}" x2="${r1(xs(v))}" y1="${m.t}" y2="${H - m.b}"/>` +
        `<text class="tick" x="${r1(xs(v))}" y="${H - m.b + 16}" text-anchor="middle">${esc(fmtTick(v))}</text>`;
    });
    if (cfg.band) {
      out += `<rect x="${r1(xs(cfg.band.lo))}" y="${m.t}" width="${r1(xs(cfg.band.hi) - xs(cfg.band.lo))}" height="${H - m.t - m.b}" fill="#e1e0d9" opacity="0.7"/>`;
    }
    out += `<line class="axis" x1="${r1(xs(0))}" x2="${r1(xs(0))}" y1="${m.t}" y2="${H - m.b}"/>`;
    const tips = [];
    list.forEach((it, i) => {
      const cy = m.t + rowH * (i + 0.5), sig = isNum(it.lo) && isNum(it.hi) && (it.lo > 0 || it.hi < 0);
      const color = it.color || SERIES[0];
      out += `<text class="lab${it.strong ? ' strong' : ''}" x="${m.l - 8}" y="${r1(cy + 4)}" text-anchor="end">${esc(it.nm)}</text>`;
      if (isNum(it.lo) && isNum(it.hi)) out += `<line x1="${r1(xs(it.lo))}" x2="${r1(xs(it.hi))}" y1="${r1(cy)}" y2="${r1(cy)}" stroke="${color}" stroke-width="2" stroke-linecap="round"/>`;
      out += sig
        ? `<circle cx="${r1(xs(it.est))}" cy="${r1(cy)}" r="5" fill="${color}" stroke="${SURFACE}" stroke-width="2"/>`
        : `<circle cx="${r1(xs(it.est))}" cy="${r1(cy)}" r="4.5" fill="${SURFACE}" stroke="${color}" stroke-width="2"/>`;
      out += `<text class="tick" x="${W - m.r + 10}" y="${r1(cy + 4)}">${esc(it.sub || '')}</text>`;
      out += `<rect data-ti="${i}" x="0" y="${r1(cy - rowH / 2)}" width="${W}" height="${rowH}" fill="transparent"/>`;
      tips.push(it.tip || { title: it.nm, rows: [{ v: fx.d3(it.est), k: '推定値' }, { v: `${fx.d3(it.lo)} 〜 ${fx.d3(it.hi)}`, k: '95%信頼区間' }] });
    });
    if (cfg.xLab) out += `<text class="axlab" x="${r1((m.l + W - m.r) / 2)}" y="${H - 4}" text-anchor="middle">${esc(cfg.xLab)}</text>`;
    out += '</svg>';
    host.innerHTML = (cfg.title ? `<div class="fbx-sub">${esc(cfg.title)}</div>` : '') + out +
      note('塗りの点 = 95%信頼区間が 0 をまたがない（有意）、白抜き = またぐ（有意でない）。' +
        (cfg.band && cfg.band.label ? esc(cfg.band.label) + '。' : ''));
    host._tips = tips;
  }

  /** ヒストグラム＋観測値の縦線（帰無分布の表示用） */
  function nullHistChart(host, centers, counts, obs, cfg = {}) {
    host._hover = null; host._leave = null;
    hoverable(host);
    const W = width(host), H = cfg.H || 240, m = { l: 44, r: 16, t: 26, b: 40 };
    const lo = Math.min(...centers, obs) - 0.02, hi = Math.max(...centers, obs) + 0.02;
    const xt = niceTicks(lo, hi, 6), xs = scale(xt[0], xt[xt.length - 1], m.l, W - m.r);
    const yt = niceTicks(0, Math.max(...counts, 1), 4, 1), ys = scale(0, yt[yt.length - 1], H - m.b, m.t);
    const bw = centers.length > 1 ? Math.abs(xs(centers[1]) - xs(centers[0])) : 10;
    let out = svgOpen(W, H) + frame(W, H, m, xs, ys, xt, yt, fmtTick, fmtTick, cfg.xLab, cfg.yLab);
    const tips = [];
    centers.forEach((c, i) => {
      const y = ys(counts[i]), h = (H - m.b) - y, x = xs(c) - bw / 2 + 1;
      if (h > 0) out += `<path d="${barPath(x, y, Math.max(1, bw - 2), h, 3)}" fill="${MUTED}"/>`;
      out += `<rect data-ti="${i}" x="${r1(xs(c) - bw / 2)}" y="${m.t}" width="${r1(bw)}" height="${H - m.t - m.b}" fill="transparent"/>`;
      tips.push({ title: `r ≈ ${c.toFixed(3)}`, rows: [{ v: `${counts[i]} 回`, k: 'シミュレーションでの出現回数' }] });
    });
    out += `<line x1="${r1(xs(obs))}" x2="${r1(xs(obs))}" y1="${m.t - 6}" y2="${H - m.b}" stroke="${SERIES[1]}" stroke-width="2"/>` +
      `<text class="lab strong" x="${r1(xs(obs))}" y="${m.t - 10}" text-anchor="middle">実際の値 r = ${obs.toFixed(3)}</text></svg>`;
    host.innerHTML = legendHTML([{ nm: '帰無モデルでの相関（シミュレーション）', color: MUTED }, { nm: '実際のデータの相関', color: SERIES[1] }]) + out;
    host._tips = tips;
  }

  // ===========================================================================
  // 7. 各章
  // ===========================================================================
  const teamOptions = (rows, withAll) =>
    (withAll ? [[-1, '全チーム']] : []).concat(rows.slice().sort((a, b) => TEAMS[a.t].localeCompare(TEAMS[b.t])).map(o => [o.t, TEAMS[o.t]]));

  // ---- 概要 ----
  function mountOverview(id) {
    const P = shell(id, ['ctl', 'pick', 'kpi', 'strip', 'chart']);
    control(P.ctl, { md: true });
    let team = FOCUS_T;
    P.pick.addEventListener('change', e => { if (e.target.classList.contains('fbx-team')) { team = +e.target.value; draw(); } });
    function draw() {
      const rg = range(), rows = standings(rg.s, rg.from, rg.to);
      if (!rows.some(o => o.t === team)) team = rows.some(o => o.t === FOCUS_T) ? FOCUS_T : (rows[0] ? rows[0].t : -1);
      P.pick.innerHTML = row('チーム', selectHTML('fbx-team', teamOptions(rows), team));
      const o = rows.find(x => x.t === team);
      if (!o) { P.kpi.innerHTML = note('この範囲の試合データがない。'); P.strip.innerHTML = ''; P.chart.innerHTML = ''; return; }
      P.kpi.innerHTML = kpis([
        { k: '順位', v: o.rank + '位', s: `${rows.length}チーム中（${rangeText(rg)}）`, main: true },
        { k: '勝点', v: fx.int(o.Pts), s: `${o.W}勝 ${o.D}分 ${o.L}敗` },
        { k: '期待勝点 xPts', v: fx.d1(o.xPts), s: `勝点との差 ${fx.s1(o.PmxP)}（xPts順位 ${o.xRank}位）` },
        { k: '得点 ／ xG', v: `${o.G} ／ ${fx.d1(o.xG)}`, s: `差 ${fx.s1(o.GmxG)}` },
        { k: '失点 ／ xGA', v: `${o.GA} ／ ${fx.d1(o.xGA)}`, s: `差 ${fx.s1(o.GAmxGA)}（マイナスなら期待より失点が少ない）` }
      ]);
      const ms = seasonRace(rg.s).filter(r => r.t === team && r.n >= rg.from && r.n <= rg.to).sort((a, b) => a.n - b.n);
      P.strip.innerHTML = '<div class="fbx-sub">試合結果（W 勝ち ／ D 引き分け ／ L 負け）</div><div class="fbx-strip">' +
        ms.map((r, i) => `<span class="fbx-res ${r.res}" data-ti="${i}" tabindex="0" aria-label="第${r.n}節 ${r.res}">${r.res}</span>`).join('') + '</div>';
      hoverable(P.strip);
      P.strip._hover = null;
      P.strip._tips = ms.map(r => ({
        title: `第${r.n}節 ${r.h ? 'ホーム' : 'アウェイ'} vs ${TEAMS[r.o]}`,
        rows: [{ v: `${r.g} - ${r.ga}`, k: 'スコア' }, { v: `${fx.d2(r.xg)} - ${fx.d2(r.xga)}`, k: 'xG' }, { v: r.d, k: '日付' }]
      }));
      let cp = 0, cx = 0;
      const pa = [], pb = [];
      ms.forEach(r => { cp += r.pt; cx += r.xp; pa.push({ x: r.n, y: cp }); pb.push({ x: r.n, y: +cx.toFixed(2) }); });
      const xv = ms.map(r => r.n);
      lineChart(P.chart, {
        series: [{ nm: '勝点（累積）', color: SERIES[0], pts: pa }, { nm: '期待勝点（累積）', color: SERIES[1], pts: pb }],
        x0: rg.from, x1: rg.to, xVals: xv, xTicks: xv, tipTitle: x => `第${x}節まで`, tipFmt: fx.d1,
        xLab: '節', yLab: '範囲内の累積', yZero: true, legend: true, endLabels: true, H: 300
      });
    }
    reg(P.root, draw);
  }

  // ---- 順位表 ----
  function mountStandings(id) {
    const P = shell(id, ['ctl', 'opt', 'table']);
    control(P.ctl, { md: true });
    let venue = 'all', view = 'basic';
    const ts = { key: 'rank', dir: 1, limit: 100 };
    P.opt.innerHTML = row('会場', selectHTML('fbx-venue', [['all', '全試合'], ['H', 'ホームのみ'], ['A', 'アウェイのみ']], venue) +
      lab('表示', true) + selectHTML('fbx-view', [['basic', '基本'], ['xg', 'xG 指標'], ['all', 'すべて']], view));
    P.opt.addEventListener('change', e => {
      if (e.target.classList.contains('fbx-venue')) venue = e.target.value;
      if (e.target.classList.contains('fbx-view')) view = e.target.value;
      draw();
    });
    wireTable(P.table, ts, draw);
    const C = {
      rank: nCol('rank', '順位', o => o.rank), team: tCol('team', 'チーム', o => TEAMS[o.t]),
      MP: nCol('MP', '試合', o => o.MP), W: nCol('W', '勝', o => o.W), D: nCol('D', '分', o => o.D), L: nCol('L', '敗', o => o.L),
      Pts: nCol('Pts', '勝点', o => o.Pts), G: nCol('G', '得点', o => o.G), GA: nCol('GA', '失点', o => o.GA),
      GD: nCol('GD', '得失点差', o => o.GD, fx.s0), xPts: nCol('xPts', 'xPts', o => o.xPts, fx.d1),
      PmxP: nCol('PmxP', '勝点−xPts', o => o.PmxP, fx.s1), xRank: nCol('xRank', 'xPts順位', o => o.xRank),
      xG: nCol('xG', 'xG', o => o.xG, fx.d1), GmxG: nCol('GmxG', '得点−xG', o => o.GmxG, fx.s1),
      xGA: nCol('xGA', 'xGA', o => o.xGA, fx.d1), GAmxGA: nCol('GAmxGA', '失点−xGA', o => o.GAmxGA, fx.s1),
      xGD: nCol('xGD', 'xGD', o => o.xGD, fx.s1)
    };
    const SETS = {
      basic: ['rank', 'team', 'MP', 'W', 'D', 'L', 'Pts', 'G', 'GA', 'GD'],
      xg: ['rank', 'team', 'MP', 'Pts', 'xPts', 'PmxP', 'xRank', 'G', 'xG', 'GmxG', 'GA', 'xGA', 'GAmxGA', 'xGD'],
      all: Object.keys(C)
    };
    function draw() {
      const rg = range();
      P.table.innerHTML = tableHTML(SETS[view].map(k => C[k]), standings(rg.s, rg.from, rg.to, venue), ts, {
        hl: o => o.t === FOCUS_T,
        foot: `${rangeText(rg)}。同勝点は得失点差→総得点の順（直接対決は考慮しない）。xPts は Understat の勝敗確率から計算。`
      });
    }
    reg(P.root, draw);
  }

  // ---- 勝点の推移 ----
  function mountRace(id) {
    const P = shell(id, ['ctl', 'opt', 'chips', 'chart']);
    control(P.ctl, { md: true });
    const METRICS = [['p', '累積勝点'], ['xp', '累積期待勝点'], ['diff', '累積（勝点−期待勝点）'], ['gd', '累積得失点差'], ['xgd', '累積xG差'], ['rank', '順位']];
    let metric = 'p', inited = false, lastS = -1;
    const pool = new Pool();
    P.opt.innerHTML = row('指標', selectHTML('fbx-metric', METRICS, metric) +
      '<span class="fbx-note2">範囲の開始節から累積する。色分けしていないチームは灰色の細線で表示。</span>');
    P.opt.addEventListener('change', e => { if (e.target.classList.contains('fbx-metric')) { metric = e.target.value; draw(); } });
    wireChips(P.chips, pool, draw, pr => {
      const rg = range(), rows = standings(rg.s, rg.from, rg.to);
      pool.clear();
      if (pr === 'top4') rows.slice(0, 4).forEach(o => pool.add(o.t));
      if (pr === 'bottom4') rows.slice(-4).forEach(o => pool.add(o.t));
      if (pr === 'focus' && FOCUS_T >= 0) pool.add(FOCUS_T);
    });
    function draw() {
      const rg = range(), rows = standings(rg.s, rg.from, rg.to);
      if (!inited || lastS !== rg.s) {
        const keep = pool.ids().filter(t => rows.some(o => o.t === t));
        if (!inited) { rows.slice(0, 4).forEach(o => pool.add(o.t)); if (FOCUS_T >= 0 && rows.some(o => o.t === FOCUS_T)) pool.add(FOCUS_T); }
        else { pool.ids().forEach(t => { if (!keep.includes(t)) pool.remove(t); }); }
        inited = true; lastS = rg.s;
      }
      P.chips.innerHTML = chipsHTML('チーム', rows.map(o => ({ id: o.t, nm: TEAMS[o.t], sub: o.rank + '位' })), pool,
        [['top4', '上位4'], ['bottom4', '下位4'], ['focus', '注目チームのみ'], ['clear', '全解除']], takeFlash(P.chips));

      const byTeam = new Map();
      seasonRace(rg.s).forEach(r => {
        if (r.n < rg.from || r.n > rg.to) return;
        if (!byTeam.has(r.t)) byTeam.set(r.t, []);
        byTeam.get(r.t).push(r);
      });
      const xs = [];
      for (let n = rg.from; n <= rg.to; n++) xs.push(n);
      const cum = new Map();
      byTeam.forEach((arr, t) => {
        arr.sort((a, b) => a.n - b.n);
        const c = { p: 0, xp: 0, gd: 0, xgd: 0, g: 0 }, m = new Map();
        arr.forEach(r => {
          c.p += r.pt; c.xp += r.xp; c.gd += r.g - r.ga; c.xgd += r.xg - r.xga; c.g += r.g;
          m.set(r.n, { p: c.p, xp: c.xp, diff: c.p - c.xp, gd: c.gd, xgd: c.xgd, g: c.g, opp: TEAMS[r.o], score: `${r.g}-${r.ga}` });
        });
        cum.set(t, m);
      });
      if (metric === 'rank') {
        xs.forEach(n => {
          const snap = [];
          cum.forEach((m, t) => {
            let last = null;
            for (let q = n; q >= rg.from; q--) if (m.has(q)) { last = m.get(q); break; }
            if (last) snap.push({ t, v: last });
          });
          snap.sort((a, b) => b.v.p - a.v.p || b.v.gd - a.v.gd || b.v.g - a.v.g);
          snap.forEach((o, i) => { if (cum.get(o.t).has(n)) cum.get(o.t).get(n).rank = i + 1; });
        });
      }
      const series = rows.map(o => {
        const m = cum.get(o.t) || new Map();
        return {
          nm: TEAMS[o.t], color: pool.color(o.t), muted: !pool.has(o.t),
          pts: xs.map(n => {
            const v = m.get(n);
            return { x: n, y: v ? +(+v[metric]).toFixed(2) : null, note: v ? `vs ${v.opp} ${v.score}` : '' };
          })
        };
      });
      const nT = rows.length;
      lineChart(P.chart, {
        series, x0: rg.from, x1: rg.to, xVals: xs, xTicks: xs, tipTitle: n => `第${n}節`,
        tipFmt: metric === 'p' || metric === 'gd' ? fx.int : metric === 'rank' ? v => v + '位' : fx.d1,
        yRev: metric === 'rank', yTicks: metric === 'rank' ? [1].concat(niceTicks(1, nT, 4, 1).filter(v => v > 1 && v <= nT)) : null,
        zeroLine: metric === 'diff' || metric === 'gd' || metric === 'xgd',
        xLab: '節', yLab: METRICS.find(x => x[0] === metric)[1], endLabels: true, legend: true, H: 380
      });
    }
    reg(P.root, draw);
  }

  // ---- チーム指標の散布図 ----
  function mountTeamScatter(id) {
    const P = shell(id, ['ctl', 'opt', 'chart', 'table']);
    control(P.ctl, { md: true });
    let xk = 'xG', yk = 'xGA', per = false, diag = false, med = true;
    const ts = { key: 'rank', dir: 1, limit: 100 };
    const items = TM.map(x => [x.k, x.lab]);
    P.opt.innerHTML = row('横軸', selectHTML('fbx-x', items, xk) + lab('縦軸', true) + selectHTML('fbx-y', items, yk)) +
      row('', chk('fbx-per', '1試合あたりに換算', per) + chk('fbx-diag', '対角線（同じ値）', diag) + chk('fbx-med', '中央値の線', med));
    P.opt.addEventListener('change', e => {
      const c = e.target.classList;
      if (c.contains('fbx-x')) xk = e.target.value;
      if (c.contains('fbx-y')) yk = e.target.value;
      if (c.contains('fbx-per')) per = e.target.checked;
      if (c.contains('fbx-diag')) diag = e.target.checked;
      if (c.contains('fbx-med')) med = e.target.checked;
      draw();
    });
    wireTable(P.table, ts, draw);
    function draw() {
      const rg = range(), rows = standings(rg.s, rg.from, rg.to);
      const fX = tmFmt(xk, per), fY = tmFmt(yk, per), lX = tmLab(xk, per), lY = tmLab(yk, per);
      const pts = rows.map(o => {
        const focus = o.t === FOCUS_T;
        return {
          x: tmVal(o, xk, per), y: tmVal(o, yk, per), color: SERIES[0], muted: !focus, r: focus ? 6 : 5,
          label: TEAMS[o.t], labelPri: focus ? 2 : 1, strong: focus,
          tip: { title: `${TEAMS[o.t]}（${o.rank}位）`, rows: [{ v: fX(tmVal(o, xk, per)), k: lX }, { v: fY(tmVal(o, yk, per)), k: lY }, { v: fx.int(o.Pts), k: '勝点' }] }
        };
      });
      const hint = xk === 'xG' && yk === 'xGA' ? { br: '右下ほど内容が良い（作って、作らせない）' } : null;
      P.chart.innerHTML = '';
      scatterChart(P.chart, { pts, xLab: lX, yLab: lY, diag, medians: med, xFmt: fmtTick, yFmt: fmtTick, H: 440, quadrants: hint });
      if (FOCUS_T >= 0) P.chart.insertAdjacentHTML('afterbegin', legendHTML([{ nm: TEAMS[FOCUS_T] + '（注目チーム）', color: SERIES[0], shape: 'dot' }, { nm: 'その他のチーム', color: MUTED, shape: 'dot' }]));
      P.table.innerHTML = tableHTML([
        nCol('rank', '順位', o => o.rank), tCol('team', 'チーム', o => TEAMS[o.t]),
        nCol('x', lX, o => tmVal(o, xk, per), fX), nCol('y', lY, o => tmVal(o, yk, per), fY)
      ], rows, ts, { hl: o => o.t === FOCUS_T, foot: rangeText(rg) });
    }
    reg(P.root, draw);
  }

  // ---- シーズン比較 ----
  function mountSeasonCompare(id) {
    const P = shell(id, ['opt', 'chips', 'chart', 'table']);
    let metric = 'Pts', per = false;
    const pool = new Pool();
    const last = SEASONS.length - 1;
    const ts = { key: 's' + last, dir: -1, limit: 100 };
    fullStandings(DEFAULT_S).slice(0, 4).forEach(o => pool.add(o.t));
    if (FOCUS_T >= 0) pool.add(FOCUS_T);
    P.opt.innerHTML = row('指標', selectHTML('fbx-metric', TM.map(x => [x.k, x.lab]), metric) + chk('fbx-per', '1試合あたり', per) +
      '<span class="fbx-note2">各シーズンの全節の数字（上の節の範囲指定は反映しない）</span>');
    P.opt.addEventListener('change', e => {
      if (e.target.classList.contains('fbx-metric')) metric = e.target.value;
      if (e.target.classList.contains('fbx-per')) per = e.target.checked;
      draw();
    });
    wireTable(P.table, ts, draw);
    // 最新シーズンの順位 → それ以前に在籍したチームの順
    const teamOrder = (() => {
      const seen = new Set(), out = [];
      for (let i = last; i >= 0; i--) fullStandings(i).forEach(o => { if (!seen.has(o.t)) { seen.add(o.t); out.push(o.t); } });
      return out;
    })();
    wireChips(P.chips, pool, draw, pr => {
      pool.clear();
      if (pr === 'top4') fullStandings(last).slice(0, 4).forEach(o => pool.add(o.t));
      if (pr === 'focus' && FOCUS_T >= 0) pool.add(FOCUS_T);
    });
    function draw() {
      const bySeason = SEASONS.map((_, i) => new Map(fullStandings(i).map(o => [o.t, o])));
      const f = tmFmt(metric, per), l = tmLab(metric, per);
      P.chips.innerHTML = chipsHTML('チーム', teamOrder.map(t => ({ id: t, nm: TEAMS[t], sub: bySeason[last].has(t) ? bySeason[last].get(t).rank + '位' : '' })), pool,
        [['top4', `上位4（${SLAB[last]}）`], ['focus', '注目チームのみ'], ['clear', '全解除']], takeFlash(P.chips));
      const idx = SEASONS.map((_, i) => i);
      const series = teamOrder.map(t => ({
        nm: TEAMS[t], color: pool.color(t), muted: !pool.has(t),
        pts: idx.map(i => {
          const o = bySeason[i].get(t);
          return { x: i, y: o ? tmVal(o, metric, per) : null, note: o ? `${o.rank}位・${o.MP}試合` : '' };
        })
      }));
      const nT = Math.max(...SEASONS.map((_, i) => bySeason[i].size), 1);
      lineChart(P.chart, {
        series, x0: 0, x1: last, xVals: idx, xTicks: idx, xFmt: i => SLAB[i], tipTitle: i => SLAB[i], tipFmt: f, yFmt: fmtTick,
        yRev: metric === 'rank', yTicks: metric === 'rank' ? [1].concat(niceTicks(1, nT, 4, 1).filter(v => v > 1 && v <= nT)) : null,
        zeroLine: ['PmxP', 'GmxG', 'GAmxGA', 'GD', 'xGD'].includes(metric),
        markers: true, endLabels: true, legend: true, yLab: l, H: 340
      });
      const rows = teamOrder.map(t => {
        const r = { t };
        idx.forEach(i => { const o = bySeason[i].get(t); r['s' + i] = o ? tmVal(o, metric, per) : null; });
        return r;
      });
      P.table.innerHTML = tableHTML([tCol('team', 'チーム', r => TEAMS[r.t])].concat(idx.map(i => nCol('s' + i, SLAB[i], r => r['s' + i], f))),
        rows, ts, { hl: r => r.t === FOCUS_T, foot: `指標：${l}。空欄はそのシーズンに所属していない。` });
    }
    reg(P.root, draw);
  }

  // ---- 選手ランキング ----
  function mountPlayers(id) {
    const P = shell(id, ['ctl', 'opt', 'chart', 'table']);
    control(P.ctl, { md: true });
    let team = -1, minSh = 10, q = '', xk = 'xg', yk = 'g', diag = true;
    const ts = { key: 'g', dir: -1, limit: 50 };
    const items = PM.map(x => [x.k, x.lab]);
    P.opt.innerHTML =
      row('チーム', '<span data-slot="team"></span>' + lab('シュート数', true) +
        `<input type="range" class="fbx-min" min="0" max="80" step="1" value="${minSh}"><b class="fbx-minv">${minSh}</b><span class="fbx-note2">本以上</span>`) +
      row('選手検索', '<input type="search" class="fbx-q" placeholder="名前の一部（例：yamal、mbappe）">' +
        '<span class="fbx-note2">一致した選手を図で強調し、表を絞り込む（アクセント記号は無視）</span>') +
      row('散布図', '横 ' + selectHTML('fbx-x', items, xk) + ' 縦 ' + selectHTML('fbx-y', items, yk) + chk('fbx-diag', '対角線', diag));
    const slot = P.opt.querySelector('[data-slot="team"]'), minv = P.opt.querySelector('.fbx-minv');
    P.opt.addEventListener('input', e => {
      if (e.target.classList.contains('fbx-min')) { minSh = +e.target.value; minv.textContent = minSh; ts.limit = 50; draw(); }
      if (e.target.classList.contains('fbx-q')) { q = norm(e.target.value.trim()); ts.limit = 50; draw(); }
    });
    P.opt.addEventListener('change', e => {
      const c = e.target.classList;
      if (c.contains('fbx-team')) team = +e.target.value;
      if (c.contains('fbx-x')) xk = e.target.value;
      if (c.contains('fbx-y')) yk = e.target.value;
      if (c.contains('fbx-diag')) diag = e.target.checked;
      if (!c.contains('fbx-min') && !c.contains('fbx-q')) draw();
    });
    wireTable(P.table, ts, draw);
    function draw() {
      const rg = range();
      const teams = standings(rg.s, rg.from, rg.to);
      if (team >= 0 && !teams.some(o => o.t === team)) team = -1;
      slot.innerHTML = selectHTML('fbx-team', teamOptions(teams, true), team);
      const all = playerStats(rg.s, rg.from, rg.to).filter(o => (team < 0 || o.t === team) && o.sh >= minSh);
      const hit = o => q && norm(PLAYERS[o.p]).includes(q);
      const fX = pmDef(xk).f, fY = pmDef(yk).f;
      const byY = all.slice().sort((a, b) => (b[yk] || 0) - (a[yk] || 0)).slice(0, 5).map(o => o.p + '|' + o.t);
      const pts = all.map(o => {
        const fi = FOCUS_P.indexOf(o.p), h = hit(o);
        const color = fi >= 0 && fi < 8 ? SERIES[fi] : h ? EMPH : MUTED;
        return {
          x: o[xk], y: o[yk], color, muted: fi < 0 && !h, r: fi >= 0 || h ? 6 : 4.5,
          label: PLAYERS[o.p], labelPri: fi >= 0 ? 3 : h ? 2 : byY.includes(o.p + '|' + o.t) ? 1 : 0, strong: fi >= 0 || h,
          tip: {
            title: `${PLAYERS[o.p]}（${TEAMS[o.t]}）`,
            rows: [{ v: fX(o[xk]), k: pmDef(xk).lab }, { v: fY(o[yk]), k: pmDef(yk).lab }, { v: `${o.sh}本`, k: 'シュート' }]
          }
        };
      });
      P.chart.innerHTML = '';
      scatterChart(P.chart, { pts, xLab: pmDef(xk).lab, yLab: pmDef(yk).lab, diag, H: 440 });
      const leg = FOCUS_P.slice(0, 8).map((p, i) => ({ nm: PLAYERS[p], color: SERIES[i], shape: 'dot' }));
      if (q) leg.push({ nm: '検索に一致', color: EMPH, shape: 'dot' });
      leg.push({ nm: 'その他', color: MUTED, shape: 'dot' });
      P.chart.insertAdjacentHTML('afterbegin', legendHTML(leg));
      const rows = q ? all.filter(hit) : all;
      P.table.innerHTML = tableHTML([
        tCol('player', '選手', o => PLAYERS[o.p]), tCol('team', 'チーム', o => TEAMS[o.t])
      ].concat(PM.map(m => nCol(m.k, m.lab, o => o[m.k], m.f))), rows, ts, {
        hl: o => FOCUS_P.includes(o.p),
        foot: `${rangeText(rg)}。アシスト・キーパス・xA はシュート直前のパスから計算（オウンゴールは除く）。`
      });
    }
    reg(P.root, draw);
  }

  // ---- シュートマップ ----
  function mountShotmap(id) {
    const P = shell(id, ['ctl', 'opt', 'chips', 'kpi', 'main']);
    control(P.ctl, { md: true });
    let mode = 'player', team = FOCUS_T, noPen = false, resF = 'all', lastPickS = -1;
    const pool = new Pool(), sitOn = SITUATIONS.map(() => true);
    const ts = { key: 'xg', dir: -1, limit: 30 };
    FOCUS_P.forEach(p => pool.add(p));
    P.opt.innerHTML =
      row('対象', selectHTML('fbx-mode', [['player', '選手（最大8人）'], ['team', 'チームのシュート'], ['against', 'チームの被シュート']], mode) +
        '<span data-slot="team"></span><span data-slot="pick"></span>') +
      row('状況', SITUATIONS.map((s, i) => chk('fbx-sit', ja(s), true, ` data-i="${i}"`)).join('') + chk('fbx-nopen', 'PKを除く', noPen)) +
      row('結果', selectHTML('fbx-res', [['all', 'すべて'], ['goal', 'ゴールのみ'], ['miss', 'ゴール以外']], resF));
    const slotTeam = P.opt.querySelector('[data-slot="team"]'), slotPick = P.opt.querySelector('[data-slot="pick"]');
    P.main.innerHTML = '<div class="fbx-flex"><div class="main" data-part="pitch"></div><div class="side" data-part="side"></div></div>';
    const pitchEl = P.main.querySelector('[data-part="pitch"]'), sideEl = P.main.querySelector('[data-part="side"]');
    P.opt.addEventListener('change', e => {
      const t = e.target, c = t.classList;
      if (c.contains('fbx-mode')) { mode = t.value; lastPickS = -1; }
      if (c.contains('fbx-team')) team = +t.value;
      if (c.contains('fbx-sit')) sitOn[+t.dataset.i] = t.checked;
      if (c.contains('fbx-nopen')) noPen = t.checked;
      if (c.contains('fbx-res')) resF = t.value;
      if (c.contains('fbx-pick')) return;
      draw();
    });
    wirePicker(P.opt, 'fbx-pick', i => {
      if (!pool.add(i)) P.chips._flash = '色分けは8人まで。どれかを外してから追加する。';
      draw();
    });
    wireChips(P.chips, pool, draw);
    wireTable(sideEl, ts, draw, 30);
    function draw() {
      const rg = range();
      const teams = standings(rg.s, rg.from, rg.to);
      if (!teams.some(o => o.t === team)) team = teams.some(o => o.t === FOCUS_T) ? FOCUS_T : (teams[0] ? teams[0].t : -1);
      const base = seasonShots(rg.s).filter(x => x.n >= rg.from && x.n <= rg.to && sitOn[x.si] && !(noPen && x.pen) &&
        (resF === 'all' || (resF === 'goal') === x.goal));
      if (mode === 'player') {
        slotTeam.innerHTML = '';
        if (lastPickS !== rg.s) {
          const names = [...new Set(seasonShots(rg.s).map(x => PLAYERS[x.p]))].filter(Boolean).sort();
          slotPick.innerHTML = pickerHTML('fbx-pick', id + '-dl', names, '選手名を入力して追加');
          lastPickS = rg.s;
        }
        P.chips.innerHTML = chipsHTML('選手', pool.ids().map(p => ({ id: p, nm: PLAYERS[p] })), pool, null, takeFlash(P.chips), true);
      } else {
        slotPick.innerHTML = ''; lastPickS = -1;
        slotTeam.innerHTML = selectHTML('fbx-team', teamOptions(teams), team);
        P.chips.innerHTML = '';
      }
      const list = mode === 'player' ? base.filter(x => pool.has(x.p)) : base.filter(x => (mode === 'team' ? x.t : x.o) === team);
      const colorOf = x => (mode === 'player' ? pool.color(x.p) : SERIES[0]);
      const sum = arr => arr.reduce((s, x) => s + x.xg, 0);
      const g = list.filter(x => x.goal).length, xg = sum(list);
      P.kpi.innerHTML = kpis([
        { k: 'シュート', v: fx.int(list.length) + '本', main: true },
        { k: 'ゴール', v: fx.int(g) },
        { k: 'xG', v: fx.d2(xg), s: `得点−xG ${fx.s2(g - xg)}` },
        { k: 'xG／シュート', v: fx.d3(div(xg, list.length)), s: '1本あたりのチャンスの質' },
        { k: '枠内率', v: fx.pct(div(list.filter(x => x.onT).length, list.length)), s: 'ゴール＋セーブされた' }
      ]);
      pitchChart(pitchEl, list, colorOf, x => ({
        title: `${PLAYERS[x.p]}（${TEAMS[x.t]}） vs ${TEAMS[x.o]}`,
        rows: [
          { v: fx.d2(x.xg), k: 'xG' }, { v: ja(x.res), k: '結果' }, { v: `${x.mi}分`, k: '時間' },
          { v: ja(SITUATIONS[x.si]), k: '状況' }, { v: `第${x.n}節`, k: '' }
        ].concat(x.a >= 0 ? [{ v: PLAYERS[x.a], k: 'アシスト（パス）' }] : [])
      }));
      const leg = mode === 'player'
        ? pool.ids().map(p => ({ nm: PLAYERS[p], color: pool.color(p), shape: 'dot' }))
        : [{ nm: mode === 'team' ? `${TEAMS[team]} のシュート` : `${TEAMS[team]} が受けたシュート`, color: SERIES[0], shape: 'dot' }];
      pitchEl.insertAdjacentHTML('afterbegin', legendHTML(leg.concat([{ nm: '輪 = ゴール以外（塗り = ゴール、大きさ = xG）', color: '#fff', shape: 'ring' }])));

      const acc = new Map();
      list.forEach(x => {
        let o = acc.get(x.p);
        if (!o) { o = { p: x.p, t: x.t, sh: 0, g: 0, xg: 0, npxg: 0 }; acc.set(x.p, o); }
        o.sh++; o.xg += x.xg; if (x.goal) o.g++; if (!x.pen) o.npxg += x.xg;
      });
      const rows = [...acc.values()];
      rows.forEach(o => { o.gmx = o.g - o.xg; o.xgps = div(o.xg, o.sh); });
      sideEl.innerHTML = '<div class="fbx-sub">シューター別</div>' + tableHTML([
        { k: 'player', lab: '選手', v: o => PLAYERS[o.p], html: o => (mode === 'player' ? `<i class="key" style="background:${pool.color(o.p)}"></i>` : '') + esc(PLAYERS[o.p]) },
        nCol('sh', '本', o => o.sh), nCol('g', '得点', o => o.g), nCol('xg', 'xG', o => o.xg, fx.d2),
        nCol('gmx', '得点−xG', o => o.gmx, fx.s2), nCol('xgps', 'xG/本', o => o.xgps, fx.d3)
      ], rows, ts, { foot: rangeText(rg) });
    }
    reg(P.root, draw);
  }

  // ---- 選手のシーズン推移 ----
  function mountPlayerTrend(id) {
    const P = shell(id, ['opt', 'chips', 'chart', 'table']);
    const METRICS = [['g', '得点'], ['xg', 'xG'], ['gmx', '得点−xG'], ['npg', '非PK得点'], ['npxg', 'npxG'], ['a', 'アシスト'], ['xa', 'xA'], ['kp', 'キーパス'], ['sh', 'シュート'], ['ga', '得点+アシスト'], ['xgxa', 'xG+xA']];
    let metric = 'g';
    const pool = new Pool();
    const ts = { key: 'player', dir: 1, limit: 20 };
    FOCUS_P.forEach(p => pool.add(p));
    const years = [...new Set(trend.map(r => r.y))].sort((a, b) => a - b);
    const agg = new Map();
    trend.forEach(r => {
      const key = r.p + '|' + r.y;
      let o = agg.get(key);
      if (!o) { o = { p: r.p, y: r.y, sh: 0, g: 0, xg: 0, npg: 0, npxg: 0, a: 0, xa: 0, kp: 0, teams: [] }; agg.set(key, o); }
      ['sh', 'g', 'xg', 'npg', 'npxg', 'a', 'xa', 'kp'].forEach(k => { o[k] += r[k] || 0; });
      if (r.tm >= 0) o.teams.push(TEAMS_ALL[r.tm]);
    });
    agg.forEach(o => { o.gmx = o.g - o.xg; o.ga = o.g + o.a; o.xgxa = o.xg + o.xa; });
    const names = [...new Set(trend.map(r => PLAYERS[r.p]))].filter(Boolean).sort();
    P.opt.innerHTML = row('指標', selectHTML('fbx-metric', METRICS, metric)) +
      row('選手を追加', pickerHTML('fbx-pick', id + '-dl', names, '選手名を入力（候補から選ぶ）') +
        '<span class="fbx-note2">Understat のシュートデータがある全シーズン</span>');
    P.opt.addEventListener('change', e => {
      const t = e.target;
      if (t.classList.contains('fbx-metric')) { metric = t.value; draw(); }
    });
    wirePicker(P.opt, 'fbx-pick', i => {
      if (!pool.add(i)) P.chips._flash = '色分けは8人まで。どれかを外してから追加する。';
      draw();
    });
    wireChips(P.chips, pool, draw);
    wireTable(P.table, ts, draw, 20);
    function draw() {
      const lb = METRICS.find(x => x[0] === metric)[1];
      const f = ['xg', 'npxg', 'xa', 'xgxa'].includes(metric) ? fx.d2 : metric === 'gmx' ? fx.s2 : fx.int;
      P.chips.innerHTML = chipsHTML('選手', pool.ids().map(p => ({ id: p, nm: PLAYERS[p] })), pool, null, takeFlash(P.chips), true);
      // 横軸は、選んだ選手のデータがある最初のシーズンから
      const first = years.findIndex(y => pool.ids().some(p => agg.has(p + '|' + y)));
      const ys = first < 0 ? years : years.slice(first);
      const idx = ys.map((_, i) => i);
      const series = pool.ids().map(p => ({
        nm: PLAYERS[p], color: pool.color(p),
        pts: ys.map((y, i) => { const o = agg.get(p + '|' + y); return { x: i, y: o ? o[metric] : null, note: o ? o.teams.join(' / ') : '' }; })
      }));
      if (!series.length) { P.chart.innerHTML = note('選手を追加すると推移を表示する。'); P.chart._hover = null; }
      else lineChart(P.chart, {
        series, x0: 0, x1: ys.length - 1, xVals: idx, xTicks: idx, xFmt: i => seasonLabelOf(ys[i]),
        tipTitle: i => seasonLabelOf(ys[i]), tipFmt: f, markers: true, endLabels: true, legend: true,
        zeroLine: metric === 'gmx', yLab: lb, H: 320
      });
      const rows = pool.ids().map(p => {
        const r = { p };
        ys.forEach(y => { const o = agg.get(p + '|' + y); r['y' + y] = o ? o[metric] : null; });
        return r;
      });
      P.table.innerHTML = tableHTML([{ k: 'player', lab: '選手', v: r => PLAYERS[r.p], html: r => `<i class="key" style="background:${pool.color(r.p)}"></i>${esc(PLAYERS[r.p])}` }]
        .concat(ys.map(y => nCol('y' + y, seasonLabelOf(y), r => r['y' + y], f))), rows, ts, { foot: `指標：${lb}。空欄はリーグ内でシュートの記録がないシーズン。` });
    }
    reg(P.root, draw);
  }

  // ---- 得点・失点の時間帯 ----
  function mountGoalTiming(id) {
    const P = shell(id, ['ctl', 'opt', 'kpi', 'chart', 'table']);
    control(P.ctl, { md: true });
    let team = FOCUS_T, show = 'both', bin = 15;
    const ts = { key: 'i', dir: 1, limit: 100 };
    P.opt.innerHTML = row('チーム', '<span data-slot="team"></span>' + lab('表示', true) +
      selectHTML('fbx-show', [['both', '得点と失点'], ['for', '得点のみ'], ['against', '失点のみ']], show) +
      lab('区切り', true) + selectHTML('fbx-bin', [[5, '5分'], [10, '10分'], [15, '15分']], bin));
    const slot = P.opt.querySelector('[data-slot="team"]');
    P.opt.addEventListener('change', e => {
      const c = e.target.classList;
      if (c.contains('fbx-team')) team = +e.target.value;
      if (c.contains('fbx-show')) show = e.target.value;
      if (c.contains('fbx-bin')) bin = +e.target.value;
      draw();
    });
    wireTable(P.table, ts, draw);
    function draw() {
      const rg = range(), teams = standings(rg.s, rg.from, rg.to);
      if (!teams.some(o => o.t === team)) team = teams.some(o => o.t === FOCUS_T) ? FOCUS_T : (teams[0] ? teams[0].t : -1);
      slot.innerHTML = selectHTML('fbx-team', teamOptions(teams), team);
      const nb = Math.round(90 / bin);
      const cats = Array.from({ length: nb }, (_, i) => `${i * bin + 1}-${(i + 1) * bin}`).concat(['90+']);
      const binOf = m => (m > 90 ? nb : Math.max(0, Math.min(nb - 1, Math.floor((Math.max(m, 1) - 1) / bin))));
      const goals = seasonShots(rg.s).filter(x => x.goal && x.n >= rg.from && x.n <= rg.to);
      const gf = goals.filter(x => x.t === team), ga = goals.filter(x => x.o === team);
      const cnt = arr => { const v = new Array(nb + 1).fill(0); arr.forEach(x => { v[binOf(x.mi)]++; }); return v; };
      const vf = cnt(gf), va = cnt(ga);
      const series = [];
      if (show !== 'against') series.push({ nm: '得点', color: SERIES[0], v: vf });
      if (show !== 'for') series.push({ nm: '失点', color: SERIES[1], v: va });
      const late = arr => arr.filter(x => x.mi >= 76).length;
      P.kpi.innerHTML = kpis([
        { k: '得点', v: fx.int(gf.length), s: `前半 ${gf.filter(x => x.mi <= 45).length} ／ 後半 ${gf.filter(x => x.mi > 45).length}`, main: true },
        { k: '76分以降の得点', v: fx.int(late(gf)), s: fx.pct(div(late(gf), gf.length)) + ' が終盤' },
        { k: '失点', v: fx.int(ga.length), s: `前半 ${ga.filter(x => x.mi <= 45).length} ／ 後半 ${ga.filter(x => x.mi > 45).length}` },
        { k: '76分以降の失点', v: fx.int(late(ga)), s: fx.pct(div(late(ga), ga.length)) + ' が終盤' }
      ]);
      barChart(P.chart, { cats, series, xLab: '時間帯（分）', yLab: '点数', legend: true, tipTitle: i => `${cats[i]}分`, H: 280 });
      const rows = cats.map((c, i) => ({ i, c, f: vf[i], a: va[i] }));
      P.table.innerHTML = tableHTML([nCol('i', '時間帯', r => r.i, i => cats[i] + '分'), nCol('f', '得点', r => r.f), nCol('a', '失点', r => r.a)], rows, ts,
        { foot: `${TEAMS[team] || ''}／${rangeText(rg)}。シュートデータのゴールから集計（オウンゴールは含まない）。45分台の追加時間は前半に入る。` });
    }
    reg(P.root, draw);
  }

  // ---- 出場時間つきスタッツ ----
  function mountTeamPlayers(id) {
    const P = shell(id, ['ctl', 'opt', 'bars', 'table']);
    control(P.ctl, { note: 'Understat のチームページのシーズン通算（節の範囲指定は反映しない）' });
    let team = FOCUS_T, minMin = 0, metric = 'mn';
    const ts = { key: 'mn', dir: -1, limit: 40 };
    const METRICS = [
      ['mn', '出場時間', fx.int], ['g', '得点', fx.int], ['xg', 'xG', fx.d2], ['a', 'アシスト', fx.int], ['xa', 'xA', fx.d2],
      ['xg90', '90分あたりxG', fx.d2], ['xa90', '90分あたりxA', fx.d2], ['xgxa90', '90分あたりxG+xA', fx.d2],
      ['xgc', 'xGChain', fx.d2], ['xgb', 'xGBuildup', fx.d2]
    ];
    P.opt.innerHTML = row('チーム', '<span data-slot="team"></span>' + lab('出場時間', true) +
      `<input type="range" class="fbx-min" min="0" max="3000" step="90" value="${minMin}"><b class="fbx-minv">${minMin}</b><span class="fbx-note2">分以上</span>`) +
      row('横棒の指標', selectHTML('fbx-metric', METRICS.map(x => [x[0], x[1]]), metric));
    const slot = P.opt.querySelector('[data-slot="team"]'), minv = P.opt.querySelector('.fbx-minv');
    P.opt.addEventListener('input', e => { if (e.target.classList.contains('fbx-min')) { minMin = +e.target.value; minv.textContent = minMin; draw(); } });
    P.opt.addEventListener('change', e => {
      if (e.target.classList.contains('fbx-team')) { team = +e.target.value; draw(); }
      if (e.target.classList.contains('fbx-metric')) { metric = e.target.value; draw(); }
    });
    wireTable(P.table, ts, draw, 40);
    tplayers.forEach(r => {
      const p90 = v => div(v * 90, r.mn);
      r.xg90 = p90(r.xg); r.xa90 = p90(r.xa); r.xgxa90 = p90((r.xg || 0) + (r.xa || 0)); r.g90 = p90(r.g); r.gmx = r.g - r.xg;
    });
    function draw() {
      const s = st.s, inS = tplayers.filter(r => r.s === s);
      const teams = [...new Set(inS.map(r => r.t))].map(t => ({ t }));
      if (!teams.some(o => o.t === team)) team = teams.some(o => o.t === FOCUS_T) ? FOCUS_T : (teams[0] ? teams[0].t : -1);
      slot.innerHTML = selectHTML('fbx-team', teamOptions(teams), team);
      const rows = inS.filter(r => r.t === team && (r.mn || 0) >= minMin);
      if (!rows.length) { P.bars.innerHTML = note('該当する選手がいない（データ未取得のシーズン・チームの可能性）。'); P.table.innerHTML = ''; return; }
      const md = METRICS.find(x => x[0] === metric);
      const top = rows.filter(r => isNum(r[metric])).sort((a, b) => b[metric] - a[metric]).slice(0, 15);
      P.bars.innerHTML = `<div class="fbx-sub">${esc(md[1])}（上位15人）</div>` +
        hbars(top.map(r => ({ nm: PLAYERS[r.pl] || '?', v: r[metric], txt: md[2](r[metric]), color: FOCUS_P.includes(r.pl) ? SERIES[1] : SERIES[0] })));
      P.table.innerHTML = tableHTML([
        tCol('player', '選手', r => PLAYERS[r.pl] || '?'), tCol('pos', 'ポジション', r => r.pos),
        nCol('gm', '試合', r => r.gm), nCol('mn', '出場時間', r => r.mn), nCol('g', '得点', r => r.g), nCol('xg', 'xG', r => r.xg, fx.d2),
        nCol('gmx', '得点−xG', r => r.gmx, fx.s2), nCol('a', 'アシスト', r => r.a), nCol('xa', 'xA', r => r.xa, fx.d2),
        nCol('npxg', 'npxG', r => r.npxg, fx.d2), nCol('xg90', 'xG/90', r => r.xg90, fx.d2), nCol('xa90', 'xA/90', r => r.xa90, fx.d2),
        nCol('xgxa90', 'xG+xA/90', r => r.xgxa90, fx.d2), nCol('sh', 'シュート', r => r.sh), nCol('kp', 'キーパス', r => r.kp),
        nCol('xgc', 'xGChain', r => r.xgc, fx.d2), nCol('xgb', 'xGBuildup', r => r.xgb, fx.d2), nCol('yc', '警告', r => r.yc), nCol('rc', '退場', r => r.rc)
      ], rows, ts, { hl: r => FOCUS_P.includes(r.pl), foot: `${TEAMS[team]}／${SLAB[s]}。/90 は90分あたり。横棒のオレンジは注目選手。` });
    }
    reg(P.root, draw);
  }

  // ---- オフサイド（チーム） ----
  function mountOffside(id) {
    const P = shell(id, ['ctl', 'opt', 'notice', 'chart', 'trendopt', 'trend', 'table']);
    control(P.ctl, { note: 'FBref／ESPN のシーズン通算（節の範囲指定は反映しない。表の「出典」列を参照）' });
    const lgOn = LEAGUES.map(() => true);
    let emph = Math.max(0, LG_I), labels = true, tmetric = 'kePm';
    const ts = { key: 'kePm', dir: -1, limit: 40 };
    const lgButtons = () => LEAGUES.map((_, i) => `<button class="fbx-lg${lgOn[i] ? ' on' : ''}" data-lg="${i}">${esc(LEAGUE_LAB[i])}</button>`).join('');
    P.opt.innerHTML = row('リーグ', '<span data-slot="lg"></span>') +
      row('強調', selectHTML('fbx-emph', LEAGUES.map((_, i) => [i, LEAGUE_LAB[i]]), emph) + chk('fbx-labels', 'チーム名を表示', labels));
    const slotLg = P.opt.querySelector('[data-slot="lg"]');
    slotLg.innerHTML = lgButtons();
    P.opt.addEventListener('click', e => {
      const b = e.target.closest('button[data-lg]');
      if (b) { const i = +b.dataset.lg; lgOn[i] = !lgOn[i]; slotLg.innerHTML = lgButtons(); draw(); }
    });
    P.opt.addEventListener('change', e => {
      if (e.target.classList.contains('fbx-emph')) emph = +e.target.value;
      if (e.target.classList.contains('fbx-labels')) labels = e.target.checked;
      draw();
    });
    const TMET = [['kePm', 'オフサイドにかけた数／試合'], ['kaPm', 'オフサイドにかかった数／試合'], ['trap', 'トラップ比率（かけた ÷ 合計）']];
    P.trendopt.innerHTML = '<div class="fbx-sub">リーグ別・シーズン推移（チーム平均）</div>' + row('指標', selectHTML('fbx-tm', TMET, tmetric) +
      '<span class="fbx-note2">白抜きの点はデータが途中までのシーズン</span>');
    P.trendopt.addEventListener('change', e => { if (e.target.classList.contains('fbx-tm')) { tmetric = e.target.value; draw(); } });
    wireTable(P.table, ts, draw, 40);
    function draw() {
      const y = SEASONS[st.s], lgs = LEAGUES.map((_, i) => i).filter(i => lgOn[i]);
      P.notice.innerHTML = coverageNotice(lgs, y);
      const rows = offT.filter(r => r.y === y && lgOn[r.lg]);
      const focusSq = fbSquadOf(st.s, FOCUS_T);
      // 色はリーグに固定（下の推移グラフと同じ色）。注目チームは大きい点と太字ラベルで示す
      const pts = rows.map(r => {
        const isE = r.lg === emph, isF = r.sq === focusSq && r.lg === LG_I;
        return {
          x: r.kaPm, y: r.kePm, color: SERIES[r.lg % SERIES.length], muted: !isE && !isF, r: isF ? 8 : 5,
          label: SQUADS[r.sq], labelPri: labels ? (isF ? 3 : isE ? 2 : 0) : (isF ? 3 : 0), strong: isF,
          tip: {
            title: `${SQUADS[r.sq]}（${LEAGUE_LAB[r.lg]}）`,
            rows: [{ v: fx.d2(r.kePm), k: 'かけた数／試合' }, { v: fx.d2(r.kaPm), k: 'かかった数／試合' }, { v: fx.d1(r.gm), k: '試合（90分換算）' }]
          }
        };
      });
      P.chart.innerHTML = '';
      scatterChart(P.chart, {
        pts, xLab: 'オフサイドにかかった数／試合', yLab: 'オフサイドにかけた数／試合', medians: true, H: 460,
        quadrants: { tl: '左上 = ハイライン守備（よくかけ、あまりかからない）', br: '右下 = 裏抜け主体の攻撃' }
      });
      const leg = [{ nm: LEAGUE_LAB[emph], color: SERIES[emph % SERIES.length], shape: 'dot' }];
      if (focusSq >= 0) leg.push({ nm: SQUADS[focusSq] + '（注目チーム・大きい点）', color: SERIES[LG_I % SERIES.length], shape: 'dot' });
      leg.push({ nm: 'その他のリーグ', color: MUTED, shape: 'dot' });
      P.chart.insertAdjacentHTML('afterbegin', legendHTML(leg));

      const years = [...new Set(offT.map(r => r.y))].sort((a, b) => a - b), idx = years.map((_, i) => i);
      const series = lgs.map(lg => ({
        nm: LEAGUE_LAB[lg], color: SERIES[lg % SERIES.length],
        pts: years.map((yy, i) => {
          const v = offT.filter(r => r.lg === lg && r.y === yy).map(r => r[tmetric]).filter(isNum);
          return { x: i, y: v.length ? v.reduce((s, q) => s + q, 0) / v.length : null, hollow: offPartial(lg, yy), note: v.length ? offSourceNote(lg, yy) : '' };
        })
      }));
      lineChart(P.trend, {
        series, x0: 0, x1: years.length - 1, xVals: idx, xTicks: idx, xFmt: i => seasonLabelOf(years[i]), tipTitle: i => seasonLabelOf(years[i]),
        tipFmt: tmetric === 'trap' ? fx.pct : fx.d2, yFmt: tmetric === 'trap' ? v => (v * 100).toFixed(0) + '%' : fmtTick,
        markers: true, endLabels: true, legend: true, yLab: TMET.find(x => x[0] === tmetric)[1], H: 320
      });
      P.table.innerHTML = tableHTML([
        tCol('lg', 'リーグ', r => LEAGUE_LAB[r.lg]), tCol('sq', 'チーム', r => SQUADS[r.sq]), nCol('gm', '試合', r => r.gm, fx.d1),
        nCol('ke', 'かけた数', r => r.ke), nCol('ka', 'かかった数', r => r.ka),
        nCol('kePm', 'かけた／試合', r => r.kePm, fx.d2), nCol('kaPm', 'かかった／試合', r => r.kaPm, fx.d2),
        nCol('diffPm', '差／試合', r => r.diffPm, fx.s2), nCol('trap', 'トラップ比率', r => r.trap, fx.pct),
        tCol('src', '出典', r => SOURCES[r.src] || '')
      ], rows, ts, { hl: r => r.sq === focusSq && r.lg === LG_I, foot: `${SLAB[st.s]}。かけた数 = 相手選手がオフサイドになった回数、かかった数 = 自チームの選手がオフサイドになった回数。` });
    }
    reg(P.root, draw);
  }

  // ---- オフサイド（選手） ----
  function mountOffsidePlayers(id) {
    const P = shell(id, ['ctl', 'opt', 'notice', 'bars', 'table']);
    control(P.ctl, { note: 'FBref／ESPN のシーズン通算（節の範囲指定は反映しない。表の「出典」列を参照）' });
    // 出場の下限は「そのリーグ・シーズンで最も出場した選手の何%か」で指定する
    // （データが途中までのシーズンでも、既定のまま選手が表示されるように）
    let lg = Math.max(0, LG_I), minPct = 25, q = '', metric = 'off';
    const ts = { key: 'off', dir: -1, limit: 40 };
    P.opt.innerHTML = row('リーグ', selectHTML('fbx-lg', LEAGUES.map((_, i) => [i, LEAGUE_LAB[i]]), lg) + lab('出場', true) +
      `<input type="range" class="fbx-min" min="0" max="90" step="5" value="${minPct}"><b class="fbx-minv">${minPct}%</b><span class="fbx-note2 fbx-minnote"></span>`) +
      row('選手検索', '<input type="search" class="fbx-q" placeholder="名前の一部">' + lab('横棒', true) +
        selectHTML('fbx-metric', [['off', 'かかった数'], ['p90', '1試合あたり']], metric));
    const minv = P.opt.querySelector('.fbx-minv'), minnote = P.opt.querySelector('.fbx-minnote');
    P.opt.addEventListener('input', e => {
      if (e.target.classList.contains('fbx-min')) { minPct = +e.target.value; minv.textContent = minPct + '%'; draw(); }
      if (e.target.classList.contains('fbx-q')) { q = norm(e.target.value.trim()); draw(); }
    });
    P.opt.addEventListener('change', e => {
      if (e.target.classList.contains('fbx-lg')) { lg = +e.target.value; draw(); }
      if (e.target.classList.contains('fbx-metric')) { metric = e.target.value; draw(); }
    });
    wireTable(P.table, ts, draw, 40);
    // 分母：FBref は 90分換算の出場、ESPN は出場試合数（途中出場も1試合）
    offP.forEach(r => { r.den = isNum(r.n90) ? r.n90 : r.apps; r.p90 = div(r.off, r.den); });
    function draw() {
      P.notice.innerHTML = coverageNotice([lg], SEASONS[st.s]);
      const inLg = offP.filter(r => r.s === st.s && r.lg === lg);
      const espn = inLg.some(r => SOURCES[r.src] === 'ESPN');
      const unit = espn ? '出場試合' : '90分換算の試合';
      const maxN = inLg.reduce((m, r) => Math.max(m, r.den || 0), 0), minN = maxN * minPct / 100;
      minnote.textContent = `＝ ${unit} ${minN.toFixed(1)} 以上（最多 ${maxN.toFixed(1)}）`;
      const rows = inLg.filter(r => (r.den || 0) >= minN && (!q || norm(FB_PLAYERS[r.pl]).includes(q)));
      const top = rows.filter(r => isNum(r[metric])).sort((a, b) => b[metric] - a[metric]).slice(0, 15);
      const perLab = espn ? '1試合あたり' : '90分あたり';
      P.bars.innerHTML = top.length
        ? `<div class="fbx-sub">${metric === 'off' ? 'オフサイドにかかった数' : perLab + 'のオフサイド'}（上位15人）</div>` +
          hbars(top.map(r => ({ nm: `${FB_PLAYERS[r.pl]}（${SQUADS[r.sq]}）`, v: r[metric], txt: metric === 'off' ? fx.int(r.off) : fx.d3(r.p90) })))
        : note('該当する選手がいない。');
      P.table.innerHTML = tableHTML([
        tCol('pl', '選手', r => FB_PLAYERS[r.pl]), tCol('sq', 'チーム', r => SQUADS[r.sq]), tCol('pos', 'ポジション', r => POSITIONS[r.pos] || ''),
        nCol('den', espn ? '出場試合' : '出場（90分換算）', r => r.den, espn ? fx.int : fx.d1),
        nCol('off', 'かかった数', r => r.off), nCol('p90', perLab, r => r.p90, fx.d3)
      ], rows, ts, {
        foot: `${LEAGUE_LAB[lg]}／${SLAB[st.s]}（出典：${espn ? 'ESPN' : 'FBref'}）。選手個人の「かけた数」は定義できないため、かかった数のみ。` +
          (espn ? 'ESPN には出場時間がないため、途中出場も1試合として数える。' : '')
      });
    }
    reg(P.root, draw);
  }

  // ---- オフサイド：同じチームのシーズン推移 ----
  function mountOffsideTeamTrend(id) {
    const P = shell(id, ['opt', 'chips', 'kpi', 'chart', 'table']);
    const METRICS = [
      ['kePm', 'オフサイドにかけた数／試合', fx.d2], ['kaPm', 'オフサイドにかかった数／試合', fx.d2],
      ['diffPm', '差（かけた − かかった）／試合', fx.s2], ['trap', 'トラップ比率（かけた ÷ 合計）', fx.pct],
      ['ke', 'かけた数（シーズン合計）', fx.int], ['ka', 'かかった数（シーズン合計）', fx.int],
      ['keRank', 'リーグ内順位：かけた数／試合', v => v + '位'], ['kaRank', 'リーグ内順位：かかった数／試合', v => v + '位']
    ];
    const RANK = ['keRank', 'kaRank'], TOTAL = ['ke', 'ka'];
    let lg = Math.max(0, LG_I), metric = 'kePm', showAvg = true;
    const pools = new Map();
    const ts = { key: 'player', dir: 1, limit: 60 };
    const years = [...new Set(offT.map(r => r.y))].sort((a, b) => a - b);

    const focusSquad = () => { for (let i = tmap.length - 1; i >= 0; i--) if (tmap[i].t === FOCUS_T && tmap[i].sq >= 0) return tmap[i].sq; return -1; };
    const latestYear = l => Math.max(...offT.filter(r => r.lg === l).map(r => r.y));
    // 「上位3」は直近の完了シーズンで選ぶ（進行中のシーズンは試合数が少なく順位がぶれる）
    const latestFullYear = l => Math.max(...offT.filter(r => r.lg === l && !offPartial(l, r.y)).map(r => r.y));
    function presetTop(l, pool, key) {
      const y = latestFullYear(l);
      offT.filter(r => r.lg === l && r.y === y && isNum(r[key])).sort((a, b) => b[key] - a[key]).slice(0, 3).forEach(r => pool.add(r.sq));
    }
    function poolFor(l) {
      if (!pools.has(l)) {
        const p = new Pool(), f = focusSquad();
        if (l === LG_I && f >= 0) p.add(f); else presetTop(l, p, 'kePm');
        pools.set(l, p);
      }
      return pools.get(l);
    }

    P.opt.innerHTML =
      row('リーグ', selectHTML('fbx-lg', LEAGUES.map((_, i) => [i, LEAGUE_LAB[i]]), lg) + lab('指標', true) +
        selectHTML('fbx-metric', METRICS.map(x => [x[0], x[1]]), metric)) +
      row('', chk('fbx-avg', 'リーグ平均を表示', showAvg) +
        '<span class="fbx-note2">白抜きの点は途中までのシーズン。昇格・降格でリーグにいないシーズンは線が途切れる。</span>');
    P.opt.addEventListener('change', e => {
      const c = e.target.classList;
      if (c.contains('fbx-lg')) lg = +e.target.value;
      if (c.contains('fbx-metric')) metric = e.target.value;
      if (c.contains('fbx-avg')) showAvg = e.target.checked;
      draw();
    });
    // 選択はリーグごとに別の Pool で持つ（wireChips は Pool 固定なので個別に配線）
    P.chips.addEventListener('click', e => {
      const b = e.target.closest('button');
      if (!b || !P.chips.contains(b)) return;
      const pool = poolFor(lg);
      if (b.dataset.id != null) {
        const id2 = +b.dataset.id;
        if (pool.has(id2)) pool.remove(id2);
        else if (!pool.add(id2)) P.chips._flash = '色分けは8チームまで。どれかを外してから追加する。';
      } else if (b.dataset.preset) {
        pool.clear();
        if (b.dataset.preset === 'topKe') presetTop(lg, pool, 'kePm');
        if (b.dataset.preset === 'topKa') presetTop(lg, pool, 'kaPm');
        if (b.dataset.preset === 'focus') { const f = focusSquad(); if (f >= 0 && lg === LG_I) pool.add(f); }
      }
      draw();
    });
    wireTable(P.table, ts, draw, 60);

    function draw() {
      const pool = poolFor(lg), md = METRICS.find(x => x[0] === metric), f = md[2];
      const rows = offT.filter(r => r.lg === lg);
      const by = new Map(rows.map(r => [r.sq + '|' + r.y, r]));
      const ly = latestYear(lg);
      // チーム一覧：最新シーズンに所属 → 在籍シーズンが多い順
      const seasonsOf = new Map();
      rows.forEach(r => seasonsOf.set(r.sq, (seasonsOf.get(r.sq) || 0) + 1));
      const squads = [...seasonsOf.keys()].sort((a, b) =>
        (by.has(b + '|' + ly) - by.has(a + '|' + ly)) || seasonsOf.get(b) - seasonsOf.get(a) || SQUADS[a].localeCompare(SQUADS[b]));
      P.chips.innerHTML = chipsHTML('チーム', squads.map(sq => ({ id: sq, nm: SQUADS[sq], sub: seasonsOf.get(sq) + '季' })), pool,
        [['topKe', `${seasonLabelOf(latestFullYear(lg))} のかけた数上位3`], ['topKa', `${seasonLabelOf(latestFullYear(lg))} のかかった数上位3`]].concat(lg === LG_I && focusSquad() >= 0 ? [['focus', '注目チームのみ']] : []).concat([['clear', '全解除']]),
        takeFlash(P.chips));

      const yrs = years.filter(y => rows.some(r => r.y === y)), idx = yrs.map((_, i) => i);
      const avgOf = y => {
        const v = rows.filter(r => r.y === y).map(r => r[metric]).filter(isNum);
        return v.length ? v.reduce((s, q) => s + q, 0) / v.length : null;
      };
      const ptOf = (sq, y, i) => {
        const r = by.get(sq + '|' + y);
        if (!r) return { x: i, y: null };
        return {
          x: i, y: r[metric], hollow: offPartial(lg, y),
          note: `${r.keRank}位/${r.nTeams}（かけた）・${fx.d1(r.gm)}試合・${offSourceNote(lg, y)}`
        };
      };
      const series = squads.map(sq => ({ nm: SQUADS[sq], color: pool.color(sq), muted: !pool.has(sq), pts: yrs.map((y, i) => ptOf(sq, y, i)) }));
      if (showAvg && !RANK.includes(metric)) {
        series.push({ nm: 'リーグ平均', color: EMPH, pts: yrs.map((y, i) => ({ x: i, y: avgOf(y), hollow: offPartial(lg, y) })) });
      }
      const nT = Math.max(...rows.map(r => r.nTeams || 0), 1);
      if (!pool.size && !(showAvg && !RANK.includes(metric))) { P.chart._hover = null; P.chart.innerHTML = note('チームを選ぶと推移を表示する。'); }
      else lineChart(P.chart, {
        series, x0: 0, x1: yrs.length - 1, xVals: idx, xTicks: idx, xFmt: i => seasonLabelOf(yrs[i]), tipTitle: i => seasonLabelOf(yrs[i]),
        tipFmt: f, yFmt: metric === 'trap' ? v => (v * 100).toFixed(0) + '%' : fmtTick,
        yRev: RANK.includes(metric), yTicks: RANK.includes(metric) ? [1].concat(niceTicks(1, nT, 4, 1).filter(v => v > 1 && v <= nT)) : null,
        zeroLine: metric === 'diffPm', yZero: TOTAL.includes(metric),
        markers: true, endLabels: true, legend: true, yLab: md[1], H: 380
      });

      // 最初に選んだチームの「最新 vs 前季」
      const main = pool.ids()[0];
      if (main != null) {
        const mine = rows.filter(r => r.sq === main).sort((a, b) => a.y - b.y);
        const last = mine[mine.length - 1];
        const prev = mine.filter(r => r.y < (last ? last.y : 0) && !offPartial(lg, r.y)).pop();
        const best = mine.filter(r => !offPartial(lg, r.y) && isNum(r[metric]))
          .sort((a, b) => (RANK.includes(metric) ? a[metric] - b[metric] : b[metric] - a[metric]))[0];
        const delta = last && prev && isNum(last[metric]) && isNum(prev[metric]) ? last[metric] - prev[metric] : null;
        P.kpi.innerHTML = kpis([
          { k: `${SQUADS[main]}：${last ? seasonLabelOf(last.y) : ''}`, v: last ? f(last[metric]) : '—', s: last ? offSourceNote(lg, last.y) : '', main: true },
          { k: prev ? `前季（${seasonLabelOf(prev.y)}）` : '前季', v: prev ? f(prev[metric]) : '—', s: prev ? `かけた数 ${prev.keRank}位 ／ かかった数 ${prev.kaRank}位` : 'リーグに所属していない' },
          { k: '前季からの変化', v: delta == null ? '—' : RANK.includes(metric) ? (delta > 0 ? `${delta}つ下降` : delta < 0 ? `${-delta}つ上昇` : '変化なし') : (metric === 'trap' ? (delta > 0 ? '+' : '') + (delta * 100).toFixed(1) + 'pt' : fx.s2(delta)) },
          { k: RANK.includes(metric) ? '最高順位のシーズン' : '最も多いシーズン', v: best ? seasonLabelOf(best.y) : '—', s: best ? f(best[metric]) : '' }
        ]);
      } else P.kpi.innerHTML = '';

      const tableRows = squads.map(sq => { const r = { sq }; yrs.forEach(y => { const o = by.get(sq + '|' + y); r['y' + y] = o ? o[metric] : null; }); return r; });
      if (!RANK.includes(metric)) {
        const a = { sq: -1 };
        yrs.forEach(y => { a['y' + y] = avgOf(y); });
        tableRows.unshift(a);
      }
      P.table.innerHTML = tableHTML(
        [{ k: 'player', lab: 'チーム', v: r => (r.sq < 0 ? ' リーグ平均' : SQUADS[r.sq]),
           html: r => (r.sq < 0 ? '<b>リーグ平均</b>' : (pool.color(r.sq) ? `<i class="key" style="background:${pool.color(r.sq)}"></i>` : '') + esc(SQUADS[r.sq])) }]
          .concat(yrs.map(y => nCol('y' + y, seasonLabelOf(y) + (offPartial(lg, y) ? '*' : ''), r => r['y' + y], f))),
        tableRows, ts,
        { hl: r => r.sq < 0 || pool.has(r.sq), foot: `${LEAGUE_LAB[lg]}／指標：${md[1]}。* は途中までのシーズン。空欄はそのリーグに所属していないシーズン。出典は ${[...new Set(rows.map(r => SOURCES[r.src]))].join('・')}。` });
    }
    reg(P.root, draw);
  }

  // ---- 相関分析：かけた数 × かかった数（条件を変えて確かめる）----
  function mountOffsideCorr(id) {
    const P = shell(id, ['opt', 'notice', 'kpi', 'judge', 'chart', 'split']);
    const NUL = NULL_SIM;
    const METHODS_C = [
      ['within', 'リーグ×シーズン内で比べる（z スコア）'], ['adj', '＋チーム力（xG差）の影響を除く'],
      ['change', '同じチームの前季からの変化'], ['raw', '素の値（全体をそのまま並べる）']
    ];
    const years = [...new Set(offT.map(r => r.y))].sort((a, b) => a - b);
    const lgOn = LEAGUES.map(() => true);
    let method = 'within', withPartial = false, y0 = years[0], y1 = years[years.length - 1];
    const lgButtons = () => LEAGUES.map((_, i) => `<button class="${lgOn[i] ? 'on' : ''}" data-lg="${i}">${esc(LEAGUE_LAB[i])}</button>`).join('');
    const yearOpts = sel => years.map(y => [y, seasonLabelOf(y)]);
    P.opt.innerHTML =
      row('分析方法', selectHTML('fbx-method', METHODS_C, method)) +
      row('リーグ', '<span data-slot="lg"></span>') +
      row('シーズン', selectHTML('fbx-y0', yearOpts(), y0) + '<span class="fbx-sep">〜</span>' + selectHTML('fbx-y1', yearOpts(), y1) +
        chk('fbx-partial', '途中までのシーズンも含める', withPartial));
    const slotLg = P.opt.querySelector('[data-slot="lg"]');
    slotLg.innerHTML = lgButtons();
    P.opt.addEventListener('click', e => {
      const b = e.target.closest('button[data-lg]');
      if (!b) return;
      const i = +b.dataset.lg;
      lgOn[i] = !lgOn[i];
      if (!lgOn.some(Boolean)) lgOn[i] = true;          // 全部は外せない
      slotLg.innerHTML = lgButtons();
      draw();
    });
    P.opt.addEventListener('change', e => {
      const c = e.target.classList;
      if (c.contains('fbx-method')) method = e.target.value;
      if (c.contains('fbx-y0')) { y0 = +e.target.value; if (y1 < y0) { y1 = y0; P.opt.querySelector('.fbx-y1').value = y1; } }
      if (c.contains('fbx-y1')) { y1 = +e.target.value; if (y0 > y1) { y0 = y1; P.opt.querySelector('.fbx-y0').value = y0; } }
      if (c.contains('fbx-partial')) withPartial = e.target.checked;
      draw();
    });

    const focusSq = (() => { for (let i = tmap.length - 1; i >= 0; i--) if (tmap[i].t === FOCUS_T && tmap[i].sq >= 0) return tmap[i].sq; return -1; })();

    /** 方法に応じて {x, y, r(元の行)} を作る */
    function points(base) {
      const g = new Map();
      base.forEach(r => { const k = r.lg + '|' + r.y; if (!g.has(k)) g.set(k, []); g.get(k).push(r); });
      const stat = new Map();
      g.forEach((arr, k) => {
        const ms = key => { const v = arr.map(r => r[key]).filter(isNum); const mu = v.reduce((s, q) => s + q, 0) / (v.length || 1); const sd = Math.sqrt(v.reduce((s, q) => s + (q - mu) ** 2, 0) / Math.max(1, v.length - 1)); return { mu, sd }; };
        stat.set(k, { ke: ms('kePm'), ka: ms('kaPm'), xg: ms('xgd') });
      });
      const z = (r, key, sk) => { const s = stat.get(r.lg + '|' + r.y)[sk]; return s.sd > 0 ? (r[key] - s.mu) / s.sd : null; };
      const dev = (r, key, sk) => r[key] - stat.get(r.lg + '|' + r.y)[sk].mu;
      if (method === 'raw') return base.map(r => ({ x: r.kePm, y: r.kaPm, r }));
      if (method === 'within') return base.map(r => ({ x: z(r, 'kePm', 'ke'), y: z(r, 'kaPm', 'ka'), r }));
      if (method === 'adj') {
        const pts = base.filter(r => isNum(r.xgd)).map(r => ({ zx: z(r, 'xgd', 'xg'), x: z(r, 'kePm', 'ke'), y: z(r, 'kaPm', 'ka'), r }))
          .filter(p => isNum(p.zx) && isNum(p.x) && isNum(p.y));
        const res = key => { const f = pearson(pts.map(p => p.zx), pts.map(p => p[key])); return pts.map(p => p[key] - (f.my + (f.slope || 0) * (p.zx - f.mx))); };
        const rx = res('x'), ry = res('y');
        return pts.map((p, i) => ({ x: rx[i], y: ry[i], r: p.r }));
      }
      // change：前季（同じリーグ・同じチーム・連続したシーズン）との差。リーグ×シーズン平均との差で比べる
      const byKey = new Map(base.map(r => [r.lg + '|' + r.sq + '|' + r.y, r]));
      return base.map(r => {
        const pv = byKey.get(r.lg + '|' + r.sq + '|' + (r.y - 1));
        return pv ? { x: dev(r, 'kePm', 'ke') - dev(pv, 'kePm', 'ke'), y: dev(r, 'kaPm', 'ka') - dev(pv, 'kaPm', 'ka'), r } : null;
      }).filter(Boolean);
    }

    function draw() {
      const base = offT.filter(r => lgOn[r.lg] && r.y >= y0 && r.y <= y1 && r.gm > 0 && isNum(r.kePm) && isNum(r.kaPm) &&
        (withPartial || !offPartial(r.lg, r.y)));
      const pts = points(base).filter(p => isNum(p.x) && isNum(p.y));
      const s = corStats(pts.map(p => p.x), pts.map(p => p.y));
      const isAll = lgOn.every(Boolean) && y0 === years[0] && y1 === years[years.length - 1] && !withPartial;

      P.notice.innerHTML = (method === 'raw'
        ? notice('<b>注意</b>：リーグ×シーズンでは「かけた数の合計 = かかった数の合計」になるため、素の値にはリーグ・シーズンの全体水準の違いによる見かけの正の相関が混ざる。')
        : '') +
        ((method === 'within' || method === 'adj') && isNum(NUL.mean)
          ? note(`<b>比較の目安</b>：チームは自分とは対戦しないため、かけやすさとかかりやすさが無関係でも r ≈ ${NUL.mean.toFixed(3)}（95%：${NUL.lo.toFixed(3)}〜${NUL.hi.toFixed(3)}）程度の負の相関は仕組み上生じる（全リーグ・全シーズンでのシミュレーション）。` +
            (isAll ? '' : '条件を絞った場合、この目安は参考値。'))
          : '');

      const unitX = method === 'raw' ? 'かけた数／試合' : method === 'change' ? 'かけた数の前季からの変化（リーグ平均との差）' : 'かけた数（リーグ×シーズン内の z スコア）';
      const unitY = method === 'raw' ? 'かかった数／試合' : method === 'change' ? 'かかった数の前季からの変化（リーグ平均との差）' : 'かかった数（リーグ×シーズン内の z スコア）';
      const slopeTxt = !s ? '—' : method === 'raw' || method === 'change'
        ? `${fx.s2(s.slope)} 回`
        : `${fx.s2(s.slope)} SD`;
      const slopeSub = method === 'raw' ? 'かけた数が1回/試合多いと、かかった数は' : method === 'change' ? '前季よりかけた数が1回/試合増えると' : 'かけた数が1SD多いと、かかった数は';
      P.kpi.innerHTML = kpis([
        { k: '相関係数 r', v: s ? s.r.toFixed(3) : '—', s: s ? `95%CI ${s.lo.toFixed(3)} 〜 ${s.hi.toFixed(3)}` : '', main: true },
        { k: 'p 値', v: s ? fmtP(s.p) : '—', s: s ? (s.p < 0.05 ? '5% 水準で有意' : '有意でない') : '' },
        { k: '順位相関 ρ（スピアマン）', v: s && isNum(s.rho) ? s.rho.toFixed(3) : '—', s: s ? fmtP(s.pRho) : '' },
        { k: '回帰の傾き', v: slopeTxt, s: slopeSub },
        { k: '件数', v: s ? `${s.n}` : '—', s: method === 'change' ? '連続した2シーズン' : 'チーム×シーズン' }
      ]);
      P.judge.innerHTML = `<div class="fbx-sub">判定</div><div class="fbx-note" style="color:#0b0b0b;font-size:13px">${esc(judgeText(s))}` +
        (s && isNum(NUL.mean) && (method === 'within' || method === 'adj') && s.p < 0.05
          ? esc(s.r < NUL.lo ? ` 仕組み上の目安（${NUL.mean.toFixed(2)}）よりも明らかに強い。` : ' ただし仕組み上生じる大きさ（目安）と区別できない。')
          : '') + '</div>';

      const cf = focusSq;
      const scat = pts.map(p => {
        const f = p.r.sq === cf && p.r.lg === LG_I;
        return {
          x: p.x, y: p.y, color: SERIES[0], muted: !f, r: f ? 5.5 : 3,
          label: f ? seasonLabelOf(p.r.y).slice(2) : '', labelPri: f ? 2 : 0, strong: f,
          tip: {
            title: `${SQUADS[p.r.sq]}（${LEAGUE_LAB[p.r.lg]} ${seasonLabelOf(p.r.y)}）`,
            rows: [{ v: fx.d2(p.r.kePm), k: 'かけた数／試合' }, { v: fx.d2(p.r.kaPm), k: 'かかった数／試合' },
              { v: `${fx.s2(p.x)} ／ ${fx.s2(p.y)}`, k: '図の値（横／縦）' }]
          }
        };
      });
      P.chart.innerHTML = '';
      scatterChart(P.chart, { pts: scat, xLab: unitX, yLab: unitY, reg: true, H: 460 });
      P.chart.insertAdjacentHTML('afterbegin', legendHTML(
        (cf >= 0 ? [{ nm: `${SQUADS[cf]}（数字はシーズン）`, color: SERIES[0], shape: 'dot' }] : [])
          .concat([{ nm: 'その他のチーム×シーズン', color: MUTED, shape: 'dot' }, { nm: '線 = 回帰直線', color: MUTED }])));

      // リーグ別・シーズン別
      const itemOf = (nm, sub) => {
        const ss = corStats(sub.map(p => p.x), sub.map(p => p.y));
        return ss ? { nm, est: ss.r, lo: ss.lo, hi: ss.hi, sub: `r ${ss.r.toFixed(2)}（n=${ss.n}）`,
          tip: { title: nm, rows: [{ v: ss.r.toFixed(3), k: '相関係数 r' }, { v: `${ss.lo.toFixed(3)} 〜 ${ss.hi.toFixed(3)}`, k: '95%CI' }, { v: fmtP(ss.p), k: '' }, { v: String(ss.n), k: '件数' }] } } : null;
      };
      const band = (method === 'within' || method === 'adj') && isNum(NUL.lo) ? { lo: NUL.lo, hi: NUL.hi, label: '灰色の帯 = 仕組み上生じる範囲の目安' } : null;
      const lgItems = [itemOf('全体', pts)].concat(LEAGUES.map((_, i) => lgOn[i] ? itemOf(LEAGUE_LAB[i], pts.filter(p => p.r.lg === i)) : null)).filter(Boolean);
      lgItems[0] && (lgItems[0].strong = true);
      const yItems = years.filter(y => y >= y0 && y <= y1).map(y => itemOf(seasonLabelOf(y), pts.filter(p => p.r.y === y))).filter(Boolean);
      P.split.innerHTML = '<div class="fbx-flex"><div class="main" data-part="lgci"></div><div class="side" data-part="yci"></div></div>';
      ciChart(P.split.querySelector('[data-part="lgci"]'), lgItems, { title: 'リーグ別の相関係数（95%信頼区間）', xLab: '相関係数 r', band });
      ciChart(P.split.querySelector('[data-part="yci"]'), yItems, { title: 'シーズン別の相関係数（95%信頼区間）', xLab: '相関係数 r', band });
    }
    reg(P.root, draw);
  }

  // ---- 相関分析：回帰モデルと頑健性（R で計算した結果）----
  function mountOffsideModels(id) {
    const P = shell(id, ['coef', 'models', 'cors', 'nullh']);
    const CORR = D.corr || {};
    const models = rowsOf(CORR.models), cors = rowsOf(CORR.cors), NUL = NULL_SIM;
    const tsM = { key: 'id', dir: 1, limit: 20 }, tsC = { key: 'i', dir: 1, limit: 20 };
    cors.forEach((r, i) => { r.i = i; });
    wireTable(P.models, tsM, draw);
    wireTable(P.cors, tsC, draw);
    function draw() {
      if (!models.length) { P.coef.innerHTML = note('分析結果が埋め込まれていない。'); return; }
      ciChart(P.coef, models.map(m => ({
        nm: m.label, est: m.est, lo: m.lo, hi: m.hi, sub: `${fx.s2(m.est)}（${fmtP(m.p)}）`,
        tip: { title: m.label, rows: [{ v: m.est.toFixed(3), k: '係数' }, { v: `${m.lo.toFixed(3)} 〜 ${m.hi.toFixed(3)}`, k: '95%CI（チーム単位のクラスタ頑健SE）' }, { v: fmtP(m.p), k: '' }, { v: `${m.n}（${m.clusters}チーム）`, k: '件数' }] }
      })), { title: '回帰係数：かけた数が1試合あたり1回多いと、かかった数は何回変わるか', xLab: '係数（回／試合）' });
      P.models.innerHTML = '<div class="fbx-sub">回帰モデルの一覧（目的変数：かかった数／試合）</div>' + tableHTML([
        { k: 'id', lab: 'モデル', v: m => m.id, f: m => m.label }, nCol('est', '係数', m => m.est, v => v.toFixed(3)),
        { k: 'ci', lab: '95%CI', v: m => m.lo, f: m => `${m.lo.toFixed(3)} 〜 ${m.hi.toFixed(3)}` },
        nCol('p', 'p 値', m => m.p, fmtP), nCol('n', '件数', m => m.n), nCol('clusters', 'チーム数', m => m.clusters),
        nCol('r2', 'R²', m => m.r2, v => v.toFixed(3)), tCol('note', '内容', m => m.note)
      ], models, tsM, { foot: '標準誤差はチーム単位のクラスタ頑健標準誤差（CR1）。同じチームの複数シーズンが似ることを考慮している。' });
      P.cors.innerHTML = '<div class="fbx-sub">相関係数の一覧</div>' + tableHTML([
        { k: 'i', lab: '分析', v: r => r.i, f: r => r.label }, nCol('r', 'r', r => r.r, v => v.toFixed(3)),
        { k: 'ci', lab: '95%CI', v: r => r.lo, f: r => `${r.lo.toFixed(3)} 〜 ${r.hi.toFixed(3)}` },
        nCol('p', 'p 値', r => r.p, fmtP), nCol('rho', 'ρ', r => r.rho, v => v.toFixed(3)),
        nCol('n', '件数', r => r.n), tCol('note', '内容', r => r.note)
      ], cors, tsC);
      if (A(NUL.centers).length) {
        P.nullh.innerHTML = '';
        nullHistChart(P.nullh, A(NUL.centers), A(NUL.counts), NUL.obs, { xLab: 'リーグ×シーズン内の相関係数 r', yLab: '回数', H: 260 });
        P.nullh.insertAdjacentHTML('afterbegin', `<div class="fbx-sub">帰無モデルとの比較（${NUL.B}回のシミュレーション）</div>`);
        P.nullh.insertAdjacentHTML('beforeend', note(
          `かけやすさとかかりやすさを無関係にしたモデルでも、r は平均 ${NUL.mean.toFixed(3)}（95%：${NUL.lo.toFixed(3)}〜${NUL.hi.toFixed(3)}）になる。` +
          `実際の値 ${NUL.obs.toFixed(3)} 以下になったのは ${NUL.B}回中 ${Math.round(NUL.p * (NUL.B + 1) - 1)}回（片側 p = ${NUL.p.toFixed(3)}）。`));
      }
    }
    reg(P.root, draw);
  }

  // ---- 統合分析（xG × オフサイド）----
  function mountJoin(id) {
    const P = shell(id, ['ctl', 'opt', 'notice', 'kpi', 'chart', 'table']);
    control(P.ctl, { note: 'xG・勝点もオフサイドもシーズン通算で結合（節の範囲指定は反映しない）' });
    const XM = [['kePm', 'オフサイドにかけた数／試合', fx.d2], ['kaPm', 'オフサイドにかかった数／試合', fx.d2], ['trap', 'トラップ比率', fx.pct]];
    const YM = [['xGA', '被xG／試合'], ['GA', '失点／試合'], ['xG', 'xG／試合'], ['Pts', '勝点／試合'], ['xGD', 'xG差／試合']];
    let xk = 'kePm', yk = 'xGA';
    const ts = { key: 'rank', dir: 1, limit: 100 };
    P.opt.innerHTML = row('横軸', selectHTML('fbx-x', XM.map(x => [x[0], x[1]]), xk) + lab('縦軸', true) + selectHTML('fbx-y', YM, yk));
    P.opt.addEventListener('change', e => {
      if (e.target.classList.contains('fbx-x')) xk = e.target.value;
      if (e.target.classList.contains('fbx-y')) yk = e.target.value;
      draw();
    });
    wireTable(P.table, ts, draw);
    function draw() {
      const s = st.s, y = SEASONS[s];
      P.notice.innerHTML = LG_I >= 0 ? coverageNotice([LG_I], y) : '';
      const stand = fullStandings(s);
      const offBySq = new Map(offT.filter(r => r.y === y && r.lg === LG_I).map(r => [r.sq, r]));
      const xd = XM.find(x => x[0] === xk), yl = YM.find(x => x[0] === yk)[1];
      const rows = stand.map(o => {
        const m = tmap.find(r => r.s === s && r.t === o.t);
        const off = m && m.sq >= 0 ? offBySq.get(m.sq) : null;
        return { t: o.t, rank: o.rank, fb: m && m.sq >= 0 ? SQUADS[m.sq] : null, method: m ? METHODS[m.m] : '—', x: off ? off[xk] : null, y: div(o[yk], o.MP) };
      });
      const pts = rows.map(r => ({
        x: r.x, y: r.y, color: SERIES[0], muted: r.t !== FOCUS_T, r: r.t === FOCUS_T ? 6 : 5,
        label: TEAMS[r.t], labelPri: r.t === FOCUS_T ? 2 : 1, strong: r.t === FOCUS_T,
        tip: { title: `${TEAMS[r.t]}（${r.rank}位）`, rows: [{ v: xd[2](r.x), k: xd[1] }, { v: fx.d2(r.y), k: yl }] }
      }));
      P.chart.innerHTML = '';
      const stats = scatterChart(P.chart, { pts, xLab: xd[1], yLab: yl, reg: true, H: 420, xFmt: xk === 'trap' ? v => (v * 100).toFixed(0) + '%' : fmtTick });
      P.chart.insertAdjacentHTML('afterbegin', legendHTML([{ nm: '線 = 回帰直線', color: MUTED }]));
      const missing = rows.filter(r => !isNum(r.x)).length;
      const rv = stats && isNum(stats.r) ? stats.r : null;
      P.kpi.innerHTML = kpis([
        { k: '相関係数 r', v: rv == null ? '—' : rv.toFixed(3), s: rv == null ? '' : Math.abs(rv) < 0.2 ? 'ほぼ無関係' : Math.abs(rv) < 0.4 ? '弱い関係' : Math.abs(rv) < 0.7 ? '中程度の関係' : '強い関係', main: true },
        { k: '対象チーム', v: `${rows.length - missing} チーム` },
        { k: '突合できなかったチーム', v: `${missing} チーム`, s: missing ? '表の赤字を確認' : '' }
      ]);
      P.table.innerHTML = tableHTML([
        nCol('rank', '順位', r => r.rank), tCol('team', 'チーム', r => TEAMS[r.t]),
        { k: 'fb', lab: 'FBref 表記', v: r => r.fb || '', html: r => (r.fb ? esc(r.fb) : '<span class="miss">未対応</span>') },
        tCol('method', '突合', r => r.method), nCol('x', xd[1], r => r.x, xd[2]), nCol('y', yl, r => r.y, fx.d2)
      ], rows, ts, { hl: r => r.t === FOCUS_T, foot: `${LEAGUE_LAB[LG_I] || ''}／${SLAB[s]}。相関は因果を意味しない。` });
    }
    reg(P.root, draw);
  }

  // ===========================================================================
  // 8. 公開 API
  // ===========================================================================
  const MOUNTS = {
    overview: mountOverview, standings: mountStandings, race: mountRace, teamScatter: mountTeamScatter,
    seasonCompare: mountSeasonCompare, players: mountPlayers, shotmap: mountShotmap, playerTrend: mountPlayerTrend,
    goalTiming: mountGoalTiming, teamPlayers: mountTeamPlayers, offside: mountOffside,
    offsideTeamTrend: mountOffsideTeamTrend, offsidePlayers: mountOffsidePlayers, join: mountJoin,
    offsideCorr: mountOffsideCorr, offsideModels: mountOffsideModels
  };
  function mount(name, id) {
    const el = document.getElementById(id);
    if (!el) return;
    try {
      if (!MOUNTS[name]) throw new Error('未定義の章: ' + name);
      MOUNTS[name](id);
    } catch (e) {
      el.className = 'fbx';
      el.innerHTML = notice('この章の描画でエラーが起きた：' + esc(e && e.message));
      if (window.console) console.error('[FBX]', name, e);
    }
  }
  return { mount, state: st, range, notify, data: D };
})();
