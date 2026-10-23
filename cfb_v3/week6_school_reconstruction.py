"""One-game, auditable official-school state reconstruction; no network calls."""
import csv
import hashlib
import json
import re
from collections import Counter
from pathlib import Path

OUT = Path('cfb_v3/output/2026/week_6_rankings_relative/20261004T163129Z/espn_supplement')
SOURCE = OUT / 'official_school_csu_boxscore_decoded.json'
URL = 'https://csurams.com/sports/football/stats/2026/oregon-state/boxscore/21973'
box = json.loads(SOURCE.read_text())['boxscore']['615469229284157']
assert (int(box['visitingTeamScore']), int(box['homeTeamScore'])) == (56, 26)
rows = [(q, d, p) for q in box['summary']['quarters'] for d in q['drives']
        for p in d['plays'] if p.get('text')]
assert len(rows) == 204 and len({p['playId'] for q, d, p in rows}) == 204
name_map = {p['name']:p['name'].split(',')[1]+' '+p['name'].split(',')[0]
            for team in ('homeTeam','visitingTeam') for p in box[team]['players']
            if p.get('name','').count(',') == 1}

def context(value):
    side, down, distance, spot = value.split(',')
    assert side in ('H', 'V')
    field, yard = spot[0], int(spot[1:])
    assert field in ('H', 'V') and 0 <= yard <= 50
    return side, int(down), int(distance), (100-yard if field == side else yard)

def category(p):
    text, kind = p['text'], p['type']
    low = text.lower()
    if kind == '#':
        if low.startswith('timeout colorado state') or low.startswith('timeout oregon state'): return 'Timeout'
        if low.startswith('end of game'): return 'End of Game'
        return None
    if kind == 'X': return None  # Fold the untimed conversion into its touchdown.
    if 'NO PLAY' in text: return 'Penalty'
    if kind == 'K': return 'Kickoff'
    if kind == 'U': return 'Punt'
    if 'fumble' in low and 'recovered by csu' in low: return 'Fumble Recovery (Opponent)'
    if 'sacked' in low: return 'Sack'
    if 'intercepted' in low: return 'Pass Interception Return'
    if 'pass incomplete' in low: return 'Pass Incompletion'
    if 'pass complete' in low: return 'Passing Touchdown' if 'TOUCHDOWN' in text else 'Pass Reception'
    if 'rush ' in low or 'kneel down' in low: return 'Rushing Touchdown' if 'TOUCHDOWN' in text else 'Rush'
    raise ValueError(f'Unclassified action {p}')

def yardage(p, kind):
    t = p['text']
    if kind in ('Timeout', 'End of Game', 'End of Half', 'Pass Incompletion', 'Pass Interception Return'): return 0
    if kind == 'Penalty':
        before, after = context(p['context']), context(p['newContext'])
        assert before[0] == after[0]
        return before[3] - after[3]
    match = re.search(r'(?:kickoff|punt) (\d+) yards', t) if kind in ('Kickoff','Punt') else None
    if match: return int(match[1])
    match = re.search(r'for loss of (\d+) yards?', t)
    if match: return -int(match[1])
    match = re.search(r'for (\d+) yards? loss', t)
    if match: return -int(match[1])
    match = re.search(r'for (\d+) yards?', t)
    if match: return int(match[1])
    raise ValueError(f'No explicit yardage: {p}')

scores = {(int(s['driveIndex']), int(s['quarter'])):s for s in box['scores']}
home_score = away_score = 0
mapped, audit, stats = [], [], Counter()
for q, d, p in rows:
    quarter, seq = int(q['number']), int(p['playId'].split(',')[-1])
    kind = category(p)
    if kind is None:
        audit.append(dict(school_play_id=p['playId'],model_id='',action='excluded_administrative_or_conversion',
                          explanation='Conversion is combined with preceding TD; other excluded rows are non-play administrative text.', text=p['text']))
        continue
    before, after = context(p['context']), context(p['newContext'])
    side, down, distance, ytg = before
    assert p['teamWithBall'] == ('CSU' if side == 'H' else 'OSU')
    if kind == 'End of Game' and quarter == 2: kind = 'End of Half'
    clock = p['clock']
    clock_source = 'school_clock_field_end_of_play_convention'
    if clock == '0':
        match = re.match(r'\((\d{2}:\d{2})\)',p['text'])
        assert match, f'Missing clock {p}'
        clock = match[1]
        clock_source = 'explicit_school_text_clock_missing_field'
    assert re.fullmatch(r'\d{1,2}:\d{2}',clock)
    minute,second = map(int,clock.split(':'))
    assert 0 <= minute <= 15 and 0 <= second < 60
    gain = yardage(p, kind)
    drive = int(p['playId'].split(',')[0])
    text = p['text']
    scoring = kind in ('Passing Touchdown','Rushing Touchdown')
    conversion_id = ''
    if scoring:
        scoring_record = scores[(drive,quarter)]
        assert scoring_record['team'] == p['teamWithBall']
        assert int(scoring_record['yards']) == gain
        assert tuple(map(int,scoring_record['clock'].split(':'))) == (minute,second)
        next_p = rows[seq+1][2]
        assert next_p['type'] == 'X' and int(next_p['playId'].split(',')[-1]) == seq+1
        text += ' ' + next_p['text']
        conversion_id = next_p['playId']
        new_home,new_away = int(scoring_record['homeScore']),int(scoring_record['visitingScore'])
        assert new_home >= home_score and new_away >= away_score
        assert (new_home-home_score)+(new_away-away_score) in (6,7)
        home_score,away_score = new_home,new_away
    # Preserve original text separately; normalize school Last,First names to
    # First Last for the existing model text parser without changing the action.
    for old_name in sorted(name_map,key=len,reverse=True):
        text = text.replace(old_name,name_map[old_name])
    model_id = '401860899' + f'{seq:06d}'  # Synthetic and chronologically ordered.
    mapped.append(dict(plays_text=text,plays_type_text=kind,plays_start_down=down,
      plays_start_distance=distance,plays_period_number=quarter,plays_id=model_id,
      plays_start_yards_to_endzone=ytg,plays_end_yards_to_endzone=after[3],
      plays_stat_yardage=gain,plays_start_yard_line=(100-ytg if side=='H' else ytg),
      plays_clock_display_value=clock,plays_start_team_id='36' if side=='H' else '204',
      plays_home_score=home_score,plays_away_score=away_score,drive_id=f'school_{drive:02d}',
      drive_start_text='',drive_start_yard_line=None,drive_end_text='',drive_end_yard_line=None,
      drive_yards=None,home_team='Colorado State',away_team='Oregon State',home_team_id='36',
      away_team_id='204',home_team_abbreviation='CSU',away_team_abbreviation='OSU',
      season=2026,wk=5,scoring=scoring,school_play_id=p['playId'],school_sequence=seq,
      school_context=p['context'],school_new_context=p['newContext'],school_text=p['text'],
      school_conversion_id=conversion_id,clock_source=clock_source,source_url=URL))
    audit.append(dict(school_play_id=p['playId'],model_id=model_id,action='school_state_reconstruction',
       explanation=f'All states from official CSU source; {clock_source}; synthetic chronological ID; ESPN states rejected.',text=p['text']))
    if p['type'] in ('R','P','E') and kind != 'Penalty':
        stat_kind = 'rush' if ('rush ' in p['text'].lower() or 'kneel down' in p['text'].lower() or 'sacked' in p['text'].lower()) else 'pass'
        stats[(side,stat_kind,'attempts')] += 1
        stats[(side,stat_kind,'yards')] += gain
        stats[(side,stat_kind,'td')] += scoring
        if stat_kind == 'pass': stats[(side,'pass','completions')] += 'pass complete' in p['text'].lower()

assert (away_score,home_score) == (56,26)
assert mapped[-1]['plays_type_text'] == 'End of Game' and mapped[-1]['plays_clock_display_value'] == '00:00'
comparison=[]
for side,team in [('H',box['homeTeam']),('V',box['visitingTeam'])]:
    for kind,source in [('rush','rushes'),('pass','passes')]:
        for measure,field in [('attempts','attempts'),('yards','yards'),('td','touchdowns')]+([('completions','completions')] if kind=='pass' else []):
            observed,expected=stats[(side,kind,measure)],int(team['totals'][source][field])
            comparison.append(dict(team=team['name'],stat=f'{kind}_{measure}',reconstructed=observed,official=expected,match=observed==expected))
            assert observed == expected, comparison[-1]

def write_csv(filename, records):
    with (OUT/filename).open('w',newline='',encoding='utf-8') as f:
        writer=csv.DictWriter(f,fieldnames=list(records[0]));writer.writeheader();writer.writerows(records)

write_csv('school_model_input.csv',mapped)
write_csv('school_reconstruction_crosswalk.csv',audit)
write_csv('school_boxscore_validation.csv',comparison)
(OUT/'school_reconstruction_qa.json').write_text(json.dumps(dict(status='canonical_school_states_validated',
    source_url=URL,source_name='official Colorado State play-by-play',canonical_rows=len(rows),
    modeled_rows=len(mapped),counted_offensive_plays=sum(stats[(s,k,'attempts')] for s in ('H','V') for k in ('rush','pass')),
    boxscore_counts_and_yards_match=True,final_away_score=away_score,final_home_score=home_score,
    input_source_sha256=hashlib.sha256(SOURCE.read_bytes()).hexdigest(),
    id_policy='Synthetic game ID plus zero-padded official school sequence; school IDs retained in crosswalk.',
    clock_policy='Official structured end-of-play clocks used, matching the existing adapter convention. One missing penalty clock uses its explicit text clock; no normal-play clocks inferred.',
    score_policy='Official scoring ledger; untimed conversion merged into immediately preceding touchdown as in ESPN combined scoring narration.',
    source_repair_policy='All retained states rebuilt from official CSU data. Corrupted ESPN states are not selected or used.',
    excluded_rows='Administrative toss, quarter-start, drive-start, ball-placement and other timeouts; conversion rows combined with TD. Actual team timeouts and end-of-half/game retained.',
    limitations='Calculated EPA is not the official published cfbfastR feed. School clocks are end-of-play clocks. Quarter 2 end-of-game wording is normalized to end-of-half.'),indent=2)+'\n')
print(f'Validated {len(mapped)} modeled rows; 127 counted offensive plays and all team rush/pass attempts, yards, TDs, completions match official box score.')
