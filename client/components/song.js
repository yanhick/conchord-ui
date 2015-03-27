import React from 'react';

import SongMeta from './song-meta';
import SongContent from './song-content';
import SongToolbar from './song-toolbar';
import Chords from './chords';
import Header from './header';

export default React.createClass({

    render: function () {


        return (
            <main>
                <aside>
                    <Header onGoHome={this.props.onGoHome}>
                        <SongToolbar {...this.props}/>
                    </Header>
                </aside>
                <SongMeta data={this.props.data.meta} />
                <Chords
                    showChords={this.props.showChords}
                    data={this.props.data.chords} />
                <SongContent data={this.props.data.content}
                             showDuplicatedChoruses={this.props.showDuplicatedChoruses}
                             fontSize={this.props.fontSize} />
            </main>
        );
    }

});
